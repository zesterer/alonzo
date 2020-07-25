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

pub struct Intrinsics<T: BaseTy, L: Clone> {
    intrinsics: HashMap<usize, Box<dyn Fn(&[Value<T, L>]) -> Value<T, L> + 'static>>,
}

impl<T: BaseTy, L: Clone> Intrinsics<T, L> {
    pub fn with(mut self, id: usize, f: impl Fn(&[Value<T, L>]) -> Value<T, L> + 'static) -> Self {
        self.intrinsics.insert(id, Box::new(f));
        self
    }

    pub fn eval(&self, i: usize, args: &[Value<T, L>]) -> Value<T, L> {
        self.intrinsics[&i](args)
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
    pub fn eval(&self, scope: &Scope<T, L>, intrinsics: &Intrinsics<T, L>) -> Result<Value<T, L>, Error> {
        Ok(match self {
            Expr::Value(v) => v.clone(),
            Expr::Lazy(v) => Value::Lazy(v.clone()),
            Expr::Intrinsic(i, params) => {
                let args = params
                    .iter()
                    .map(|p| p.eval(scope, intrinsics))
                    .collect::<Result<Vec<_>, _>>()?;
                intrinsics.eval(*i, &args)
            },
            Expr::Apply(f, param) => {
                let arg = param.eval(scope, intrinsics)?;

                match f.eval(scope, intrinsics)? {
                    Value::Func(l, body) => body.eval(&scope.with(l.clone(), arg), intrinsics)?,
                    _ => return Err(Error::NotAFunction),
                }
            },
            Expr::Func(l, body) => Value::Func(l.clone(), body.clone()),
        })
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
        let expr = Expr::<_, ()>::Intrinsic(0, vec![
            TyNode::new(Expr::Value(Value::Base(3)), Ty::Base(Integer)),
            TyNode::new(Expr::Value(Value::Base(5)), Ty::Base(Integer)),
        ]);

        let result = expr.eval(
            &Scope::None,
            &Intrinsics::default().with(0, |values| Value::Base(*values[0].expect_base() + *values[1].expect_base()))
        ).unwrap();

        assert_eq!(*result.expect_base(), 8);
    }
}
