use crate::*;

#[derive(Debug)]
pub enum Error {
    NotAFunction,
}

pub enum Scope<'a, T: BaseTy, L: Clone> {
    None,
    Program(&'a Program<T, L>),
    Local(L, Value<T, L>, &'a Self),
}

impl<'a, T: BaseTy, L: Clone> Scope<'a, T, L> {
    pub fn with<'b>(&'b self, label: L, val: Value<T, L>) -> Scope<'b, T, L> where 'a: 'b {
        Scope::Local(label, val, self)
    }
}

pub trait IntrinsicFn<T: BaseTy, L: Clone> = Fn(&Scope<T, L>, &Intrinsics<T, L>, &[Value<T, L>]) -> Result<Value<T, L>, Error> + 'static;

pub struct Intrinsics<T: BaseTy, L: Clone> {
    intrinsics: HashMap<usize, Box<dyn IntrinsicFn<T, L>>>,
}

impl<T: BaseTy, L: Clone> Intrinsics<T, L> {
    pub fn with(mut self, id: usize, f: impl IntrinsicFn<T, L>) -> Self {
        self.intrinsics.insert(id, Box::new(f));
        self
    }

    pub fn eval(&self, i: usize, args: &[Value<T, L>], scope: &Scope<T, L>) -> Result<Value<T, L>, Error> {
        self.intrinsics[&i](scope, self, args)
    }
}

impl<T: BaseTy, L: Clone> Default for Intrinsics<T, L> {
    fn default() -> Self {
        Self {
            intrinsics: HashMap::default(),
        }
    }
}

impl<T: BaseTy, L: Clone> Expr<T, L> {
    pub fn eval(self: &TyNode<Self, T>, scope: &Scope<T, L>, intrinsics: &Intrinsics<T, L>) -> Result<Value<T, L>, Error> {
        Ok(match &**self {
            Expr::Value(v) => v.clone(),
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
                    Value::Lazy(expr) => match &**expr {
                        Expr::Func(l, body) => body.eval(&scope.with(l.clone(), arg), intrinsics)?,
                        _ => return Err(Error::NotAFunction),
                    },
                    _ => return Err(Error::NotAFunction),
                }
            },
            Expr::Func(l, body) => Value::Lazy(Box::new(self.clone())),
        })
    }
}

impl<T: BaseTy, L: Clone> Value<T, L> {
    pub fn to_base(self, scope: &Scope<T, L>, intrinsics: &Intrinsics<T, L>) -> Result<T::Value, Error> {
        match self {
            Value::Base(x) => Ok(x.clone()),
            Value::Lazy(x) => x.eval(scope, intrinsics)?.to_base(scope, intrinsics),
            _ => panic!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone)]
    struct Integer;

    impl BaseTy for Integer {
        type Value = i64;
    }

    #[test]
    fn basic() {
        let expr = TyNode::new(Expr::<_, ()>::Intrinsic(0, vec![
            TyNode::new(Expr::Lazy(Box::new(TyNode::new(Expr::Value(Value::Base(3)), Ty::Base(Integer)))), Ty::Base(Integer)),
            TyNode::new(Expr::Value(Value::Base(5)), Ty::Base(Integer)),
        ]), Ty::Base(Integer));

        let result = expr.eval(
            &Scope::None,
            &Intrinsics::default()
                .with(0, |scope: &Scope<'_, _, _>, intrinsics: &_, values: &[Value<_, _>]| {
                    let a = values[0].clone().to_base(scope, intrinsics)?;
                    let b = values[1].clone().to_base(scope, intrinsics)?;
                    Ok(Value::Base(a + b))
                })
        ).unwrap();

        assert_eq!(*result.expect_base(), 8);
    }
}
