use crate::*;

#[derive(Debug)]
pub enum Error {
    NoSuchDef,
    NoSuchBinding,
    NotAFunction,
    NotABaseValue,
    NoMatchingPattern,
    TypeMismatch,
}

/// Represents a value in a language.
#[derive(Debug)]
pub enum Value<L: Lang> {
    // A base primitive value
    Base(L::BaseVal),
    // A lazily-expressed value
    Lazy(Box<TyNode<Expr<L>, L>>),
    // A function
    Func(L::Ident, Box<TyNode<Expr<L>, L>>, Vec<(L::Ident, Self)>),
    // A product type value
    Product(Vec<Self>),
    // A sum type variant value
    Variant(usize, Box<Self>),
    // A list value
    List(Vec<Self>),
}

// Grr bad automatic derive bounds
impl<L: Lang> Clone for Value<L> {
    fn clone(&self) -> Self {
        match self {
            Value::Base(x) => Value::Base(x.clone()),
            Value::Lazy(x) => Value::Lazy(x.clone()),
            Value::Func(param, body, env) => Value::Func(param.clone(), body.clone(), env.clone()),
            Value::Product(xs) => Value::Product(xs.clone()),
            Value::Variant(tag, x) => Value::Variant(*tag, x.clone()),
            Value::List(xs) => Value::List(xs.clone()),
        }
    }
}

impl<L: Lang> Value<L> {
    pub fn expect_base(&self) -> &L::BaseVal {
        match self {
            Value::Base(x) => x,
            _ => panic!(),
        }
    }
}

/// Represents values available in the current scope.
pub enum Scope<'a, L: Lang> {
    None,
    Program(&'a Program<L>),
    Local(L::Ident, Value<L>, &'a Self),
    Many(Vec<(L::Ident, Value<L>)>, &'a Self),
}

impl<'a, L: Lang> Scope<'a, L> {
    /// Bind a value, creating a new scope where the binding is valid.
    pub fn with<'b>(&'b self, label: L::Ident, val: Value<L>) -> Scope<'b, L> where 'a: 'b {
        Scope::Local(label, val, self)
    }

    /// Bind many values, creating a new scope where their bindings are valid.
    pub fn with_many<'b>(&'b self, many: Vec<(L::Ident, Value<L>)>) -> Scope<'b, L> where 'a: 'b {
        Scope::Many(many, self)
    }

    /// Search for the value corresponding to a binding in this scope.
    pub fn find(&self, label: &L::Ident) -> Option<Value<L>> {
        match self {
            Scope::None => None,
            Scope::Program(prog) => prog.defs
                .get(label)
                .cloned()
                .map(Box::new)
                .map(Value::Lazy), // TODO
            Scope::Local(local, val, _) if local == label => Some(val.clone()),
            Scope::Local(_, _, parent) => parent.find(label),
            Scope::Many(many, parent) => many
                .iter()
                .rev()
                .find(|(local, _)| local == label)
                .map(|(_, val)| val.clone())
                .or_else(|| parent.find(label)),
        }
    }
}

pub trait IntrinsicFn<L: Lang> = Fn(&Scope<L>, &Intrinsics<L>, &[Value<L>]) -> Result<Value<L>, Error> + 'static;

/// A store of the intrinsic functions supported by a language.
pub struct Intrinsics<L: Lang> {
    intrinsics: HashMap<usize, Box<dyn IntrinsicFn<L>>>,
}

impl<L: Lang> Intrinsics<L> {
    /// Add a new intrinsic function with the given ID.
    pub fn with(mut self, id: usize, f: impl IntrinsicFn<L>) -> Self {
        self.intrinsics.insert(id, Box::new(f));
        self
    }

    fn eval(&self, i: usize, args: &[Value<L>], scope: &Scope<L>) -> Result<Value<L>, Error> {
        self.intrinsics[&i](scope, self, args)
    }
}

impl<L: Lang> Default for Intrinsics<L> {
    fn default() -> Self {
        Self {
            intrinsics: HashMap::default(),
        }
    }
}

impl<L: Lang> Expr<L> {
    /// Evaluate this expression within the context of a scope and a series of supported intrinsics.
    pub fn eval(self: &TyNode<Self, L>, scope: &Scope<L>, intrinsics: &Intrinsics<L>) -> Result<Value<L>, Error> {
        Ok(match &**self {
            Expr::BaseVal(v) => Value::Base(v.clone()),
            Expr::Product(fields) => fields
                .iter()
                .map(|field| field.eval(scope, intrinsics))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Product)?,
            Expr::Variant(tag, inner) => Value::Variant(*tag, Box::new(inner.eval(scope, intrinsics)?)),
            Expr::List(elements) => elements
                .iter()
                .map(|field| field.eval(scope, intrinsics))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::List)?,
            Expr::Binding(binding) => scope
                .find(binding)
                .ok_or_else(|| Error::NoSuchBinding)?,
            Expr::Lazy(v) => Value::Lazy(v.clone()),
            Expr::Intrinsic(i, params) => {
                let args = params
                    .iter()
                    .map(|p| p.eval(scope, intrinsics))
                    .collect::<Result<Vec<_>, _>>()?;
                intrinsics.eval(*i, &args, scope)?
            },
            Expr::Apply(f, param) => {
                let arg = param.eval(scope, intrinsics)?;

                match f.eval(scope, intrinsics)? {
                    Value::Func(param, body, env) => {
                        let scope = scope.with_many(env);
                        let scope = scope.with(param.clone(), arg);
                        body.eval(&scope, intrinsics)?
                    },
                    _ => return Err(Error::NotAFunction),
                }
            },
            Expr::Func(l, body) => {
                let env = self
                    .get_binding_deps()
                    .into_iter()
                    .map(|binding| {
                        let val = scope.find(&binding)?;
                        Some((binding, val))
                    })
                    .collect::<Option<_>>()
                    .ok_or_else(|| Error::NoSuchBinding)?;
                Value::Func(l.clone(), body.clone(), env)
            },
            Expr::Match(x, arms) => {
                let x = x.eval(scope, intrinsics)?;

                for (pat, body) in arms {
                    if let Some(bindings) = pat.try_extract(&x, scope, intrinsics)? {
                        return body.eval(&scope.with_many(bindings), intrinsics);
                    }
                }

                return Err(Error::NoMatchingPattern);
            },
        })
    }
}

impl<L: Lang> Pat<L> {
    fn try_extract_inner(
        &self,
        scope: &Scope<L>,
        intrinsics: &Intrinsics<L>,
        val: &Value<L>,
        bindings: &mut Vec<(L::Ident, Value<L>)>,
    ) -> Result<bool, Error> {
        match (self, val) {
            (Pat::Wildcard, _) => Ok(true),
            (Pat::Expr(expr), val) => expr.eval(scope, intrinsics)?.matches(val, scope, intrinsics),
            (Pat::Product(xs), Value::Product(ys)) if xs.len() == ys.len() => xs
                .iter()
                .zip(ys.iter())
                .try_fold(true, |a, (x, y)| {
                    Ok(a && x.try_extract_inner(scope, intrinsics, y, bindings)?)
                }),
            (Pat::Variant(tag_x, x), Value::Variant(tag_y, y)) => if tag_x == tag_y {
                x.try_extract_inner(scope, intrinsics, y, bindings)
            } else {
                Ok(false)
            },
            (Pat::List(xs, bounded), Value::List(ys)) if (!*bounded && xs.len() >= ys.len()) || xs.len() == ys.len() => xs
                .iter()
                .zip(ys.iter())
                .try_fold(true, |a, (x, y)| {
                    Ok(a && x.try_extract_inner(scope, intrinsics, y, bindings)?)
                }),
            (Pat::Bind(name, x), y) => {
                bindings.push((name.clone(), y.clone()));
                x.try_extract_inner(scope, intrinsics, y, bindings)
            },
            _ => Err(Error::TypeMismatch),
        }
    }

    fn try_extract(
        &self,
        val: &Value<L>,
        scope: &Scope<L>,
        intrinsics: &Intrinsics<L>,
    ) -> Result<Option<Vec<(L::Ident, Value<L>)>>, Error> {
        let mut bindings = Vec::new();
        let matches = self.try_extract_inner(scope, intrinsics, val, &mut bindings)?;
        Ok(if matches { Some(bindings) } else { None })
    }
}

impl<L: Lang> Value<L> {
    fn matches(&self, other: &Self, scope: &Scope<L>, intrinsics: &Intrinsics<L>) -> Result<bool, Error> {
        match (self, other) {
            // First, force evaluation of lazies
            (Value::Lazy(x), y) => x.eval(scope, intrinsics)?.matches(y, scope, intrinsics),
            (x, Value::Lazy(y)) => x.matches(&y.eval(scope, intrinsics)?, scope, intrinsics),

            (Value::Base(x), Value::Base(y)) => Ok(x == y),
            (Value::Func(_, _, _), Value::Func(_, _, _)) => Ok(false), // Function values never match
            (Value::Product(xs), Value::Product(ys)) if xs.len() == ys.len() => xs
                .iter()
                .zip(ys.iter())
                .try_fold(true, |a, (x, y)| {
                    Ok(a && x.matches(y, scope, intrinsics)?)
                }),
            (Value::Variant(tag_x, x), Value::Variant(tag_y, y)) => if tag_x == tag_y {
                x.matches(y, scope, intrinsics)
            } else {
                Ok(false)
            },
            (Value::List(xs), Value::List(ys)) => if xs.len() == ys.len() {
                xs
                    .iter()
                    .zip(ys.iter())
                    .try_fold(true, |a, (x, y)| {
                        Ok(a && x.matches(y, scope, intrinsics)?)
                    })
            } else {
                Ok(false)
            },
            _ => Err(Error::TypeMismatch),
        }
    }

    /// Attempt to extract a base value from this value. If the value is expressed lazily, this function will force its
    /// evaluation.
    pub fn into_base(self, scope: &Scope<L>, intrinsics: &Intrinsics<L>) -> Result<L::BaseVal, Error> {
        match self {
            Value::Base(x) => Ok(x.clone()),
            Value::Lazy(x) => x.eval(scope, intrinsics)?.into_base(scope, intrinsics),
            _ => Err(Error::NotABaseValue),
        }
    }
}

impl<L: Lang> Program<L> {
    /// Evaluate the given definition within the context of this program and a series of supported intrinsics.
    pub fn eval_def(&self, name: &L::Ident, intrinsics: &Intrinsics<L>) -> Result<Value<L>, Error> {
        self
            .def(name)
            .ok_or_else(|| Error::NoSuchDef)?
            .eval(
                &Scope::Program(self),
                intrinsics,
            )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestLang;

    impl Lang for TestLang {
        type BaseTy = Integer;
        type BaseVal = i64;

        type Ident = usize;
    }

    #[derive(Clone, PartialEq, Debug)]
    struct Integer;

    impl BaseTy for Integer {
        type Value = i64;
    }

    const ADD: usize = 0;

    #[test]
    fn basic() {
        let expr = TyNode::new(Expr::<TestLang>::Intrinsic(ADD, vec![
            TyNode::new(Expr::Lazy(Box::new(TyNode::new(Expr::Value(Value::Base(3)), Ty::Base(Integer)))), Ty::Base(Integer)),
            TyNode::new(Expr::Value(Value::Base(5)), Ty::Base(Integer)),
        ]), Ty::Base(Integer));

        let result = expr.eval(
            &Scope::None,
            &Intrinsics::default()
                .with(ADD, |scope: &Scope<'_, _>, intrinsics: &_, values: &[Value<_>]| {
                    let a = values[0].clone().into_base(scope, intrinsics)?;
                    let b = values[1].clone().into_base(scope, intrinsics)?;
                    Ok(Value::Base(a + b))
                })
        ).unwrap();

        assert_eq!(*result.expect_base(), 8);
    }
}
