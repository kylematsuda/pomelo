use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

/// Index into [`Arena`]
#[derive(Debug, PartialEq, Eq)]
pub struct Idx<T> {
    inner: usize,
    _ph: PhantomData<fn() -> T>,
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner,
            _ph: PhantomData,
        }
    }
}

impl<T> Copy for Idx<T> {}

impl<T> Hash for Idx<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

impl<T> Idx<T> {
    pub fn as_inner(&self) -> usize {
        self.inner
    }

    fn new(idx: usize) -> Self {
        Idx {
            inner: idx,
            _ph: PhantomData,
        }
    }
}

/// Vec-based arena type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arena<T> {
    inner: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self { inner: Vec::new() }
    }
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    pub fn reserve(&mut self, n: usize) {
        self.inner.reserve(n)
    }

    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    pub fn alloc(&mut self, val: T) -> Idx<T> {
        self.inner.push(val);
        Idx::new(self.inner.len() - 1)
    }

    pub fn items(&self) -> impl Iterator + '_ {
        self.inner.iter()
    }

    pub fn items_mut(&mut self) -> impl Iterator + '_ {
        self.inner.iter_mut()
    }

    /// This does not return an [`Option`] because we assume the index is valid.
    /// This should be alright, because we don't provide a way to construct
    /// an `Idx` on its own.
    pub fn get(&self, idx: Idx<T>) -> &T {
        self.inner.get(idx.as_inner()).expect("index is valid")
    }

    pub fn get_mut(&mut self, idx: Idx<T>) -> &mut T {
        self.inner.get_mut(idx.as_inner()).expect("index is valid")
    }
}
