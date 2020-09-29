use crate::*;

#[derive(Debug)]
pub enum Error {
    NoSuchDef,
    NoSuchBinding,
    NotAFunction,
    NotABaseValue,
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
            Expr::Value(v) => v.clone(),
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
            Expr::Match(_, _) => todo!(),
        })
    }
}

impl<L: Lang> Value<L> {
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
