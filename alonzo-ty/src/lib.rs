use std::ops::Deref;

pub trait BaseTy: Clone {
    type Value: Clone;
}

#[derive(Clone)]
pub enum Ty<T: BaseTy> {
    Base(T),
    Func(Box<Self>, Box<Self>),
    Product(Vec<Self>),
    Sum(Vec<Self>),
}

#[derive(Clone)]
pub struct TyNode<I, T: BaseTy> {
    inner: I,
    ty: Ty<T>,
}

impl<I, T: BaseTy> TyNode<I, T> {
    pub fn new(inner: I, ty: Ty<T>) -> Self {
        Self { inner, ty }
    }
}

impl<I, T: BaseTy> Deref for TyNode<I, T> {
    type Target = I;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
