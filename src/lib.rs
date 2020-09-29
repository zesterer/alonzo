#![feature(arbitrary_self_types, trait_alias)]

/// Reference implementation of program evaluation.
pub mod eval;

use std::{
    collections::HashMap,
    ops::Deref,
    cmp::{PartialEq, Eq},
    fmt::{self, Debug},
    hash::Hash,
};

/// Definitions that allow a compiler frontend to communicate with `alonzo`.
pub trait Lang {
    /// A type representing the primitives supported by the language.
    ///
    /// This type goes hand-in-hand with [`Lang::BaseVal`].
    ///
    /// # Example
    ///
    /// ```
    /// enum PrimitiveTy {
    ///     Null,
    ///     Int,
    ///     Float,
    ///     Char,
    /// }
    /// ```
    type BaseTy: Clone + PartialEq + Debug;

    /// A type representing primitive values supported by the language.
    ///
    /// This type goes hand-in-hand with [`Lang::BaseTy`].
    ///
    /// # Example
    ///
    /// ```
    /// enum PrimitiveVal {
    ///     Null,
    ///     Int(i64),
    ///     Float(f64),
    ///     Char(char),
    /// }
    /// ```
    type BaseVal: Clone + PartialEq + Debug;

    /// A type representing an identifier produced by the parser.
    ///
    /// Although using `String`/`&str` is valid, some form of interning is recommended for performance.
    type Ident: Clone + Hash + Eq + Debug;
}

/// Represents a value in a language.
#[derive(Debug)]
pub enum Value<L: Lang> {
    // A base primitive value
    Base(L::BaseVal),
    // A lazily-expressed value
    Lazy(Box<TyNode<Expr<L>, L>>),
    // A function
    Func(L::Ident, Box<TyNode<Expr<L>, L>>, Vec<(L::Ident, Self)>),
    // A product type value
    Product(Vec<Self>),
    // A sum type variant value
    Variant(usize, Box<Self>),
    // A list value
    List(Vec<Self>),
}

// Grr bad automatic derive bounds
impl<L: Lang> Clone for Value<L> {
    fn clone(&self) -> Self {
        match self {
            Value::Base(x) => Value::Base(x.clone()),
            Value::Lazy(x) => Value::Lazy(x.clone()),
            Value::Func(param, body, env) => Value::Func(param.clone(), body.clone(), env.clone()),
            Value::Product(xs) => Value::Product(xs.clone()),
            Value::Variant(tag, x) => Value::Variant(*tag, x.clone()),
            Value::List(xs) => Value::List(xs.clone()),
        }
    }
}

impl<L: Lang> Value<L> {
    pub fn expect_base(&self) -> &L::BaseVal {
        match self {
            Value::Base(x) => x,
            _ => panic!(),
        }
    }
}

/// Represents a type in a language.
#[derive(Debug)]
pub enum Ty<L: Lang> {
    // A base primitive type
    Base(L::BaseTy),
    // A function type, mapping one type to another
    Func(Box<Self>, Box<Self>),
    // A product type
    Product(Vec<Self>),
    // A sum type
    Sum(Vec<Self>),
    // A list type
    List(Box<Self>),
}

// Grr bad automatic derive bounds
impl<L: Lang> Clone for Ty<L> {
    fn clone(&self) -> Self {
        match self {
            Ty::Base(x) => Ty::Base(x.clone()),
            Ty::Func(i, o) => Ty::Func(i.clone(), o.clone()),
            Ty::Product(xs) => Ty::Product(xs.clone()),
            Ty::Sum(xs) => Ty::Sum(xs.clone()),
            Ty::List(x) => Ty::List(x.clone()),
        }
    }
}

// Grr bad automatic derive bounds
impl<L: Lang> PartialEq for Ty<L> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Ty::Base(x), Ty::Base(y)) => x == y,
            (Ty::Func(i0, o0), Ty::Func(i1, o1)) => i0 == i1 && o0 == o1,
            (Ty::Product(xs), Ty::Product(ys)) => xs == ys,
            (Ty::Sum(xs), Ty::Sum(ys)) => xs == ys,
            (Ty::List(x), Ty::List(y)) => x == y,
            _ => false,
        }
    }
}

/// Represents an expression node in the AST.
///
/// Note that `let _ = ... in ...` does not exist since it may be represented with either `match ... in { _ => ... }`
/// or `(_ -> ...)(...)`
#[derive(Debug)]
pub enum Expr<L: Lang> {
    /// Evaluate a concrete value
    Value(Value<L>),
    /// Evaluate a bound value
    Binding(L::Ident),
    /// Evaluate a lazily-expressed term
    Lazy(Box<TyNode<Self, L>>),
    /// Evaluate a program intrinsic
    Intrinsic(usize, Vec<TyNode<Self, L>>),
    /// Evaluate the application of a function with another term
    Apply(Box<TyNode<Self, L>>, Box<TyNode<Self, L>>),
    /// Evaluate a function constructor
    Func(L::Ident, Box<TyNode<Self, L>>),
    /// Evaluate a match by comparing an expression against many pattern/expression arms
    Match(Box<TyNode<Self, L>>, Vec<(TyNode<Pat<L>, L>, TyNode<Self, L>)>),
    /// Evaluate a product type variant constructor
    Product(Vec<TyNode<Self, L>>),
    /// Evaluate a sum type variant constructor
    Variant(usize, Box<TyNode<Self, L>>),
    /// Evaluate a list constructor
    List(Vec<TyNode<Self, L>>),
}

impl<L: Lang> Expr<L> {
    fn get_binding_deps_inner(&self, accounted_for: &mut Vec<L::Ident>, deps: &mut Vec<L::Ident>) {
        match self {
            Expr::Value(_) => {},
            Expr::Binding(b) if !accounted_for.contains(b) => deps.push(b.clone()),
            Expr::Binding(_) => {},
            Expr::Lazy(e) => e.get_binding_deps_inner(accounted_for, deps),
            Expr::Intrinsic(_, es) => es
                .iter()
                .for_each(|e| e.get_binding_deps_inner(accounted_for, deps)),
            Expr::Apply(f, arg) => {
                f.get_binding_deps_inner(accounted_for, deps);
                arg.get_binding_deps_inner(accounted_for, deps);
            },
            Expr::Func(param, body) => {
                accounted_for.push(param.clone());
                body.get_binding_deps_inner(accounted_for, deps);
                accounted_for.pop();
            },
            Expr::Match(e, arms) => {
                e.get_binding_deps_inner(accounted_for, deps);
                for (pat, arm) in arms {
                    // TODO: pat.get_binding_deps_inner(accounted_for, deps);
                    let bindings = pat.get_binding_outputs();
                    let old_len = accounted_for.len();
                    accounted_for.extend(bindings);
                    arm.get_binding_deps_inner(accounted_for, deps);
                    accounted_for.resize_with(old_len, || unreachable!());
                }
            },
            Expr::Product(es) => es
                .iter()
                .for_each(|e| e.get_binding_deps_inner(accounted_for, deps)),
            Expr::Variant(_, e) => e.get_binding_deps_inner(accounted_for, deps),
            Expr::List(es) => es
                .iter()
                .for_each(|e| e.get_binding_deps_inner(accounted_for, deps)),
        }
    }

    /// Return the environment of this expression (i.e: the bindings, from the parent scope, that are required to
    /// evaluate it).
    pub fn get_binding_deps(&self) -> Vec<L::Ident> {
        let mut accounted_for = Vec::new();
        let mut deps = Vec::new();
        self.get_binding_deps_inner(&mut accounted_for, &mut deps);
        deps
    }
}

// Grr bad automatic derive bounds
impl<L: Lang> Clone for Expr<L> {
    fn clone(&self) -> Self {
        match self {
            Expr::Value(x) => Expr::Value(x.clone()),
            Expr::Binding(binding) => Expr::Binding(binding.clone()),
            Expr::Lazy(x) => Expr::Lazy(x.clone()),
            Expr::Intrinsic(id, xs) => Expr::Intrinsic(*id, xs.clone()),
            Expr::Apply(f, arg) => Expr::Apply(f.clone(), arg.clone()),
            Expr::Func(param, body) => Expr::Func(param.clone(), body.clone()),
            Expr::Match(x, arms) => Expr::Match(x.clone(), arms.clone()),
            Expr::Product(xs) => Expr::Product(xs.clone()),
            Expr::Variant(tag, x) => Expr::Variant(*tag, x.clone()),
            Expr::List(xs) => Expr::List(xs.clone()),
        }
    }
}

/// Represents a pattern in the AST.
#[derive(Debug)]
pub enum Pat<L: Lang> {
    /// Match against anything
    Wildcard,
    /// Match against a pre-evaluated expression
    Expr(Box<TyNode<Expr<L>, L>>),
    /// Match against the fields of a product type
    Product(Vec<TyNode<Self, L>>),
    /// Match against a sum type variant
    Variant(usize, Box<TyNode<Self, L>>),
    /// Match against the elements of a (potentially bounded) list
    // TODO: Tail matches?
    List(Vec<TyNode<Self, L>>, bool),
    /// Match against a pattern while binding that pattern
    Bind(L::Ident, Box<TyNode<Self, L>>),
}

impl<L: Lang> Pat<L> {
    pub fn get_binding_outputs_inner(&self, bindings: &mut Vec<L::Ident>) {
        match self {
            Pat::Wildcard => {},
            Pat::Expr(_) => {},
            Pat::Product(xs) => xs
                .iter()
                .for_each(|x| x.get_binding_outputs_inner(bindings)),
            Pat::Variant(_, x) => x.get_binding_outputs_inner(bindings),
            Pat::List(xs, _) => xs
                .iter()
                .for_each(|x| x.get_binding_outputs_inner(bindings)),
            Pat::Bind(name, x) => {
                bindings.push(name.clone());
                x.get_binding_outputs_inner(bindings);
            },
        }
    }

    pub fn get_binding_outputs(&self) -> Vec<L::Ident> {
        let mut bindings = Vec::new();
        self.get_binding_outputs_inner(&mut bindings);
        bindings
    }
}

// Grr bad automatic derive bounds
impl<L: Lang> Clone for Pat<L> {
    fn clone(&self) -> Self {
        match self {
            Pat::Wildcard => Pat::Wildcard,
            Pat::Expr(x) => Pat::Expr(x.clone()),
            Pat::Product(xs) => Pat::Product(xs.clone()),
            Pat::Variant(tag, x) => Pat::Variant(*tag, x.clone()),
            Pat::List(xs, bounded) => Pat::List(xs.clone(), *bounded),
            Pat::Bind(binding, x) => Pat::Bind(binding.clone(), x.clone()),
        }
    }
}

/// Represents a typed node in the AST.
pub struct TyNode<I, L: Lang> {
    inner: I,
    ty: Ty<L>,
}

// Grr bad automatic derive bounds
impl<I: Clone, L: Lang> Clone for TyNode<I, L> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            ty: self.ty.clone(),
        }
    }
}

impl<I, L: Lang> TyNode<I, L> {
    pub fn new(inner: I, ty: Ty<L>) -> Self {
        Self {
            inner,
            ty,
        }
    }
}

impl<I, L: Lang> Deref for TyNode<I, L> {
    type Target = I;
    fn deref(&self) -> &Self::Target { &self.inner }
}

impl<I: Debug, L: Lang> fmt::Debug for TyNode<I, L> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?}", self.inner)
    }
}

pub struct Program<L: Lang> {
    pub(crate) defs: HashMap<L::Ident, TyNode<Expr<L>, L>>,
}

impl<L: Lang> Default for Program<L> {
    fn default() -> Self {
        Self {
            defs: HashMap::default(),
        }
    }
}

impl<L: Lang> Program<L> {
    /// Insert a definition with the given name.
    pub fn with_def(mut self, name: L::Ident, body: TyNode<Expr<L>, L>) -> Self {
        self.insert(name, body);
        self
    }

    /// Insert a definition with the given name.
    pub fn insert(&mut self, name: L::Ident, body: TyNode<Expr<L>, L>) {
        self.defs.insert(name, body);
    }

    /// Get a reference to the definition with the given name, should it exist.
    pub fn def(&self, name: &L::Ident) -> Option<&TyNode<Expr<L>, L>> {
        self.defs.get(name)
    }
}
