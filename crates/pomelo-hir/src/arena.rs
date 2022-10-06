use std::marker::PhantomData;

/// Index into [`Arena`]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Idx<T> {
    inner: usize,
    _ph: PhantomData<fn() -> T>,
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

    pub fn get(&self, idx: Idx<T>) -> Option<&T> {
        self.inner.get(idx.as_inner())
    }

    pub fn get_mut(&mut self, idx: Idx<T>) -> Option<&mut T> {
        self.inner.get_mut(idx.as_inner())
    }
}
