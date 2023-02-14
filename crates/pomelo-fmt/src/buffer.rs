//! Buffer implementation for Oppen algorithm.
use std::collections::VecDeque;
use std::ops::{Index, IndexMut};

#[derive(Debug, Clone)]
pub struct Buffer<T> {
    offset: usize,
    storage: VecDeque<T>,
}

impl<T> Buffer<T> {
    pub fn new() -> Self {
        Self {
            offset: 0,
            storage: VecDeque::new(),
        }
    }

    pub fn push_right(&mut self, elem: T) -> usize {
        let index = self.offset + self.storage.len();
        self.storage.push_back(elem);
        index
    }

    pub fn left(&self) -> usize {
        self.offset
    }

    pub fn left_elem(&self) -> Option<&T> {
        self.storage.front()
    }

    pub fn pop_left(&mut self) -> Option<T> {
        self.offset += 1;
        self.storage.pop_front()
    }

    pub fn clear(&mut self) {
        self.storage.clear();
    }

    pub fn is_empty(&self) -> bool {
        self.storage.is_empty()
    }
}

impl<T> Index<usize> for Buffer<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.storage[index.checked_sub(self.offset).unwrap()]
    }
}

impl<T> IndexMut<usize> for Buffer<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.storage[index.checked_sub(self.offset).unwrap()]
    }
}
