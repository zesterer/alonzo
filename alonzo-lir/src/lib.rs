use std::collections::HashMap;

use alonzo_ty::{BaseTy, TyNode};

#[derive(Clone)]
pub enum Value<T: BaseTy> {
    Base(T::Value),
    Product(Vec<Self>),
    Variant(usize, Box<Self>),
}

#[derive(Clone)]
pub enum Expr<T: BaseTy, L: Clone> {
    Value(Value<T>),
    Intrinsic(usize, Vec<TyNode<Self, T>>),
    Apply(Box<TyNode<Self, T>>, Vec<TyNode<Self, T>>),
    Func(Vec<L>, Box<TyNode<Self, T>>),
}

pub struct Program<T: BaseTy, L: Clone> {
    defs: HashMap<L, TyNode<Expr<T, L>, T>>,
}
