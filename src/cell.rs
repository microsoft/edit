//! Like `RefCell`, but without any runtime checks in release mode.

#[cfg(debug_assertions)]
pub use debug::*;
#[cfg(not(debug_assertions))]
pub use release::*;

#[allow(unused)]
#[cfg(debug_assertions)]
mod debug {
    pub type SemiRefCell<T> = std::cell::RefCell<T>;
    pub type Ref<'b, T> = std::cell::Ref<'b, T>;
    pub type RefMut<'b, T> = std::cell::RefMut<'b, T>;
}

#[cfg(not(debug_assertions))]
mod release {
    #[derive(Default)]
    #[repr(transparent)]
    pub struct SemiRefCell<T>(std::cell::UnsafeCell<T>);

    impl<T> SemiRefCell<T> {
        #[inline(always)]
        pub const fn new(value: T) -> Self {
            Self(std::cell::UnsafeCell::new(value))
        }

        #[inline(always)]
        pub const fn as_ptr(&self) -> *mut T {
            self.0.get()
        }

        #[inline(always)]
        pub const fn borrow(&self) -> Ref<'_, T> {
            Ref(unsafe { &*self.0.get() })
        }

        #[inline(always)]
        pub const fn borrow_mut(&self) -> RefMut<'_, T> {
            RefMut(unsafe { &mut *self.0.get() })
        }
    }

    #[repr(transparent)]
    pub struct Ref<'b, T>(&'b T);

    impl<'b, T> Ref<'b, T> {
        #[inline(always)]
        pub fn clone(orig: &Ref<'b, T>) -> Ref<'b, T> {
            Ref(orig.0)
        }
    }

    impl<'b, T> std::ops::Deref for Ref<'b, T> {
        type Target = T;

        #[inline(always)]
        fn deref(&self) -> &Self::Target {
            self.0
        }
    }

    #[repr(transparent)]
    pub struct RefMut<'b, T>(&'b mut T);

    impl<'b, T> std::ops::Deref for RefMut<'b, T> {
        type Target = T;

        #[inline(always)]
        fn deref(&self) -> &Self::Target {
            self.0
        }
    }

    impl<'b, T> std::ops::DerefMut for RefMut<'b, T> {
        #[inline(always)]
        fn deref_mut(&mut self) -> &mut Self::Target {
            self.0
        }
    }
}
