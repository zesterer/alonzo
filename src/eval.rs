use crate::*;

#[derive(Debug)]
pub enum Error {
    NotAFunction,
    NoSuchBinding,
}

pub enum Scope<'a, L: Lang> {
    None,
    Program(&'a Program<L>),
    Local(L::Ident, Value<L>, &'a Self),
    Many(Vec<(L::Ident, Value<L>)>, &'a Self),
}

impl<'a, L: Lang> Scope<'a, L> {
    pub fn with<'b>(&'b self, label: L::Ident, val: Value<L>) -> Scope<'b, L> where 'a: 'b {
        Scope::Local(label, val, self)
    }

    pub fn with_many<'b>(&'b self, many: Vec<(L::Ident, Value<L>)>) -> Scope<'b, L> where 'a: 'b {
        Scope::Many(many, self)
    }

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

pub struct Intrinsics<L: Lang> {
    intrinsics: HashMap<usize, Box<dyn IntrinsicFn<L>>>,
}

impl<L: Lang> Intrinsics<L> {
    pub fn with(mut self, id: usize, f: impl IntrinsicFn<L>) -> Self {
        self.intrinsics.insert(id, Box::new(f));
        self
    }

    pub fn eval(&self, i: usize, args: &[Value<L>], scope: &Scope<L>) -> Result<Value<L>, Error> {
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
            Expr::Match(_, _) => todo!(),
        })
    }
}

impl<L: Lang> Value<L> {
    pub fn into_base(self, scope: &Scope<L>, intrinsics: &Intrinsics<L>) -> Result<L::BaseVal, Error> {
        match self {
            Value::Base(x) => Ok(x.clone()),
            Value::Lazy(x) => x.eval(scope, intrinsics)?.into_base(scope, intrinsics),
            Value::Func(_, _, _) => panic!("Expected base value, found function value"),
            Value::Product(xs) => panic!("Expected base value, found product ({}) value", xs.len()),
            Value::Variant(i, _) => panic!("Expected base value, found variant ({}) value", i),
        }
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
