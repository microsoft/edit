use std::rc::{Rc, Weak};

pub struct Interner<T> {
    list: Vec<Weak<T>>,
}

impl<T> Default for Interner<T> {
    fn default() -> Self {
        Interner { list: Vec::new() }
    }
}

impl<T: PartialEq> Interner<T> {
    pub fn extract(&self) -> Vec<Rc<T>> {
        self.list.iter().filter_map(Weak::upgrade).collect()
    }

    pub fn intern(&mut self, value: T) -> Rc<T> {
        if let Some(rc) = self
            .list
            .iter()
            .filter_map(|w| w.upgrade())
            .find(|c| **c == value)
        {
            return rc;
        }

        let rc = Rc::new(value);
        self.list.push(Rc::downgrade(&rc));
        rc
    }
}
