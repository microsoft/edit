use std::ops::{Deref, DerefMut};

#[derive(Debug, Default, Clone, Copy)]
pub struct Generational<T> {
    generation: usize,
    data: T,
}

impl<T> Generational<T> {
    pub fn new(data: T) -> Self {
        Self { generation: 0, data }
    }
}

impl<T> Deref for Generational<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for Generational<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.generation = self.generation.wrapping_add(1);
        &mut self.data
    }
}

impl<T> PartialEq for Generational<T> {
    fn eq(&self, other: &Self) -> bool {
        self.generation == other.generation
    }
}

impl<T> PartialEq<Generation> for Generational<T> {
    fn eq(&self, other: &Generation) -> bool {
        self.generation == other.0
    }
}

impl<T> Eq for Generational<T> {}

pub struct Generation(usize);

impl<T> PartialEq<Generational<T>> for Generation {
    fn eq(&self, other: &Generational<T>) -> bool {
        self.0 == other.generation
    }
}
