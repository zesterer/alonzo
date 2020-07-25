pub mod eval;

use std::{
    collections::HashMap,
    ops::Deref,
};

pub trait BaseTy: Clone {
    type Value: Clone;
}

#[derive(Clone)]
pub enum Value<T: BaseTy, L: Clone> {
    Base(T::Value),
    Lazy(Box<TyNode<Expr<T, L>, T>>),
    Func(L, Box<TyNode<Expr<T, L>, T>>),
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
pub enum Ty<T: BaseTy> {
    Base(T),
    Func(Box<Self>, Box<Self>),
    Product(Vec<Self>),
    Sum(Vec<Self>),
}

#[derive(Clone)]
pub enum Expr<T: BaseTy, L: Clone> {
    Value(Value<T, L>),
    Lazy(Box<TyNode<Self, T>>),
    Intrinsic(usize, Vec<TyNode<Self, T>>),
    Apply(Box<TyNode<Self, T>>, Box<TyNode<Self, T>>),
    Func(L, Box<TyNode<Self, T>>),
}

#[derive(Clone)]
pub struct TyNode<I, T: BaseTy> {
    inner: I,
    ty: Ty<T>,
}

impl<I, T: BaseTy> TyNode<I, T> {
    pub fn new(inner: I, ty: Ty<T>) -> Self {
        Self {
            inner,
            ty,
        }
    }
}

impl<I, T: BaseTy> Deref for TyNode<I, T> {
    type Target = I;
    fn deref(&self) -> &Self::Target { &self.inner }
}

pub struct Program<T: BaseTy, L: Clone> {
    defs: HashMap<L, TyNode<Expr<T, L>, T>>,
}
