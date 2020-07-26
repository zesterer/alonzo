#![feature(arbitrary_self_types, trait_alias)]

pub mod eval;
pub mod lower;

use std::collections::HashMap;

use alonzo_ty::{BaseTy, TyNode};

#[derive(Clone)]
pub enum Value<T: BaseTy, L: Clone> {
    Base(T::Value),
    Lazy(Box<TyNode<Expr<T, L>, T>>),
    Product(Vec<Self>),
    Variant(usize, Box<Self>),
}

impl<T: BaseTy, L: Clone> Value<T, L> {
    pub fn expect_base(&self) -> &T::Value {
        match self {
            Value::Base(x) => x,
            _ => panic!(),
        }
    }
}

#[derive(Clone)]
pub enum Expr<T: BaseTy, L: Clone> {
    Value(Value<T, L>),
    Lazy(Box<TyNode<Self, T>>),
    Intrinsic(usize, Vec<TyNode<Self, T>>),
    Apply(Box<TyNode<Self, T>>, Box<TyNode<Self, T>>),
    Func(L, Box<TyNode<Self, T>>),
}

pub struct Program<T: BaseTy, L: Clone> {
    defs: HashMap<L, TyNode<Expr<T, L>, T>>,
}
