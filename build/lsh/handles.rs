use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::slice;

pub struct HandleVec<H, T> {
    list: Vec<T>,
    _handle: PhantomData<H>,
}

impl<H, T> HandleVec<H, T>
where
    H: Into<usize> + From<usize>,
{
    pub fn push(&mut self, value: T) -> H {
        let l = self.list.len();
        self.list.push(value);
        H::from(l)
    }

    pub fn indices(&self) -> impl DoubleEndedIterator<Item = H> + use<H, T> {
        (0..self.list.len()).map(H::from)
    }

    pub fn enumerate(&self) -> impl DoubleEndedIterator<Item = (H, &T)> {
        self.list.iter().enumerate().map(|(i, v)| (H::from(i), v))
    }
}

impl<H, T> Default for HandleVec<H, T> {
    fn default() -> Self {
        HandleVec { list: Vec::new(), _handle: PhantomData }
    }
}

impl<H, T> Deref for HandleVec<H, T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.list
    }
}

impl<H, T> DerefMut for HandleVec<H, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.list
    }
}

impl<'a, H, T> IntoIterator for &'a HandleVec<H, T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, H, T> IntoIterator for &'a mut HandleVec<H, T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<H, T> Index<H> for HandleVec<H, T>
where
    H: Into<usize>,
{
    type Output = T;

    fn index(&self, index: H) -> &Self::Output {
        &self.list[index.into()]
    }
}

impl<H, T> IndexMut<H> for HandleVec<H, T>
where
    H: Into<usize>,
{
    fn index_mut(&mut self, index: H) -> &mut Self::Output {
        &mut self.list[index.into()]
    }
}

macro_rules! declare_handle {
    ($vis:vis $name:ident($type:ident)) => {
        #[repr(transparent)]
        #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        $vis struct $name(pub $type);

        impl $name {
            pub const MIN: Self = Self($type::MIN);
            pub const MAX: Self = Self($type::MAX);
        }

        impl From<$name> for usize {
            fn from(value: $name) -> Self {
                value.0.try_into().unwrap()
            }
        }

        impl From<usize> for $name {
            fn from(value: usize) -> Self {
                Self(value.try_into().unwrap())
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }
    };
}

pub(crate) use declare_handle;
