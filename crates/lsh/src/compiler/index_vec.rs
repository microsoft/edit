//! Vectors indexed by distinct, checked index types.

use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

pub trait IndexType: Copy {
    fn from_usize(index: usize) -> Self;
    unsafe fn from_usize_unchecked(index: usize) -> Self;
    fn to_usize(self) -> usize;
}

macro_rules! index_type {
    ($vis:vis struct $name:ident($inner:ty)) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $vis struct $name($inner);

        impl $crate::compiler::index_vec::IndexType for $name {
            fn from_usize(index: usize) -> Self {
                Self(index.try_into().expect("index exceeds u32::MAX"))
            }

            unsafe fn from_usize_unchecked(index: usize) -> Self {
                Self(index as $inner)
            }

            fn to_usize(self) -> usize {
                self.0 as usize
            }
        }
    };
}

pub(super) use index_type;

pub struct IndexVec<I, T> {
    values: Vec<T>,
    index: PhantomData<fn(I) -> I>,
}

impl<I: IndexType, T> IndexVec<I, T> {
    pub fn new() -> Self {
        Self { values: Vec::new(), index: PhantomData }
    }

    pub fn from_elem(value: T, len: usize) -> Self
    where
        T: Clone,
    {
        if len != 0 {
            I::from_usize(len - 1);
        }
        Self { values: vec![value; len], index: PhantomData }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn push(&mut self, value: T) -> I {
        let index = I::from_usize(self.values.len());
        self.values.push(value);
        index
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (I, &T)> {
        self.values
            .iter()
            .enumerate()
            .map(|(index, value)| (unsafe { I::from_usize_unchecked(index) }, value))
    }
}

impl<I: IndexType, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I: IndexType, T> Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &T {
        &self.values[index.to_usize()]
    }
}

impl<I: IndexType, T> IndexMut<I> for IndexVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut T {
        &mut self.values[index.to_usize()]
    }
}
