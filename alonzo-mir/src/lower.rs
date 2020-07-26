use crate::{Expr, Value};

use alonzo_lir as lir;
use alonzo_ty::BaseTy;

impl<T: BaseTy, L: Clone> Expr<T, L> {
    pub fn lower(self) -> lir::Expr<T, L> {
        match self {
            Expr::Value(value) => {
                let value = match value {
                    Value::Base(base) => lir::Value::Base(base),
                    Value::Lazy(expr) => {
                        return lir::Expr::Func(vec![], Box::new(expr.map(Self::lower)))
                    }
                    _ => todo!(),
                };
                lir::Expr::Value(value)
            }

            Expr::Lazy(expr) => lir::Expr::Func(vec![], Box::new(expr.map(Self::lower))),

            Expr::Intrinsic(id, args) => lir::Expr::Intrinsic(
                id,
                args.into_iter().map(|arg| arg.map(Self::lower)).collect(),
            ),

            Expr::Func(arg, body) => {
                let mut args = vec![arg];
                let mut body = *body;

                while let Expr::Func(arg, new_body) = body.inner {
                    body = *new_body;
                    args.push(arg);
                }

                lir::Expr::Func(args, Box::new(body.map(Self::lower)))
            }

            Expr::Apply(func, arg) => {
                let mut func = *func;
                let mut args = vec![arg.map(Self::lower)];

                while let Expr::Apply(new_func, arg) = func.inner {
                    func = *new_func;
                    args.push(arg.map(Self::lower));
                }

                args.reverse();

                lir::Expr::Apply(Box::new(func.map(Self::lower)), args)
            }
        }
    }
}
