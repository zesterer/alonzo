use alonzo::*;

#[derive(Clone, PartialEq, Debug)]
struct Num;

#[derive(Clone, PartialEq, Debug)]
struct Rusty;

impl Lang for Rusty {
    type BaseTy = Num;
    type BaseVal = f64;

    type Ident = &'static str;
}

const ADD: usize = 0;
const MUL: usize = 1;

macro_rules! pat {
    // Paren pat
    (( $($x:tt)* )) => { pat!($($x)*) };
    // List
    ([ $($x:tt),* $(,)? ]) => {
        TyNode::<_, Rusty>::new(
            Pat::<Rusty>::List(vec![
                $( pat!($x) ),*
            ], true),
            Ty::Base(Num),
        )
    };
    // Product
    ({ $($x:tt),* $(,)? }) => {
        TyNode::<_, Rusty>::new(
            Pat::<Rusty>::Product(vec![
                $( pat!($x) ),*
            ]),
            Ty::Base(Num),
        )
    };
    // Numeric literal
    ($x:literal) => {
        TyNode::<_, Rusty>::new(
            Pat::<Rusty>::Expr(Box::new(TyNode::new(
                Expr::Value(Value::Base($x)),
                Ty::Base(Num),
            ))),
            Ty::Base(Num),
        )
    };
}

macro_rules! expr {
    // Let
    (let $name:ident = $val:tt in $($tail:tt)*) => {
        expr!($val : |$name| $($tail)*)
    };
    // Match
    (match $val:tt in { $( $pat:tt => $arm:tt ),* $(,)? }) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Match(
                Box::new(expr!($val)),
                vec![$( (pat!($pat), expr!($arm)) ),*],
            ),
            Ty::Base(Num),
        )
    };
    // Call
    ($arg:tt : $($f:tt)*) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Apply(Box::new(expr!($($f)*)), Box::new(expr!($arg))),
            Ty::Base(Num),
        )
    };
    // Paren expr
    (( $($x:tt)* )) => { expr!($($x)*) };
    // List
    ([ $($x:tt),* $(,)? ]) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::List(vec![
                $( expr!($x) ),*
            ]),
            Ty::Base(Num),
        )
    };
    // Product
    ({ $($x:tt),* $(,)? }) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Product(vec![
                $( expr!($x) ),*
            ]),
            Ty::Base(Num),
        )
    };
    // Numeric literal
    ($x:literal) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Value(Value::Base($x)),
            Ty::Base(Num),
        )
    };
    ($x:ident) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Binding(stringify!($x)),
            Ty::Base(Num),
        )
    };
    // Arithmetic
    ($x:tt + $($y:tt)*) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Intrinsic(ADD, vec![expr!($x), expr!($($y)*)]),
            Ty::Base(Num),
        )
    };
    ($x:tt * $($y:tt)*) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Intrinsic(MUL, vec![expr!($x), expr!($($y)*)]),
            Ty::Base(Num),
        )
    };
    // Functions
    (|$p:ident| $($body:tt)*) => {
        TyNode::<_, Rusty>::new(
            Expr::<Rusty>::Func(stringify!($p), Box::new(expr!($($body)*))),
            Ty::Base(Num),
        )
    };
}

fn main() {
    let expr = expr! {
        let add = (|a| |b| a + b) in
        let x = 5.0 in
        let y = 7.0 in
        let add_five = (x:add) in
        match { (y:add_five), (x:add_five) } in {
            { 10.0, 10.0 } => 0.0,
            { 12.0, 10.0 } => 100.0,
        }
    };

    // println!("{:#?}", expr);

    let result = expr.eval(
        &eval::Scope::None,
        &eval::Intrinsics::default()
            .with(ADD, |scope: &eval::Scope<'_, _>, intrinsics: &_, values: &[Value<_>]| {
                let a = values[0].clone().into_base(scope, intrinsics)?;
                let b = values[1].clone().into_base(scope, intrinsics)?;
                Ok(Value::Base(a + b))
            })
            .with(MUL, |scope: &eval::Scope<'_, _>, intrinsics: &_, values: &[Value<_>]| {
                let a = values[0].clone().into_base(scope, intrinsics)?;
                let b = values[1].clone().into_base(scope, intrinsics)?;
                Ok(Value::Base(a * b))
            }),
    ).unwrap();

    println!("{:#?}", result);
}
