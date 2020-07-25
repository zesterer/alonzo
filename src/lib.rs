pub trait BaseTy {
    type Value;
}

pub enum Value<T: BaseTy, L> {
    Base(T::Value),
    Func(L, Box<TyNode<Expr<T, L>, T>>),
    Product(Vec<Self>),
    Variant(usize, Box<Self>),
}

pub enum Ty<T> {
    Base(T),
    Func(Box<Self>, Box<Self>),
    Product(Vec<Self>),
    Sum(Vec<Self>),
}

pub enum Expr<T: BaseTy, L> {
    Value(Value<T, L>),
    Lazy(Box<TyNode<Self, T>>),
    Apply(Box<TyNode<Self, T>>, Box<TyNode<Self, T>>),
    Func(L, Box<TyNode<Self, T>>),
}

pub struct TyNode<I, T> {
    inner: I,
    ty: Ty<T>,
}
