// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#[cfg(feature = "superluminal")]
mod imp {
    use std::marker::PhantomData;

    /// Set the name of the current thread to the specified thread name.
    pub fn set_current_thread_name(name: &str) {
        unsafe {
            superluminal_perf_sys::PerformanceAPI_SetCurrentThreadName_N(
                name.as_ptr() as *const i8,
                name.len() as u16,
            );
        }
    }

    /// Begin an instrumentation event that ends on scope exit.
    ///
    /// # Example
    ///
    /// ```
    /// // Automatically uses the function name as the event ID.
    /// fn foo() {
    ///     instrument!();
    /// }
    ///
    /// // Creates an event explicitly named "abc" with the given additional info/data.
    /// fn bar() {
    ///     instrument!("abc", data = "hello");
    /// }
    /// ```
    #[macro_export]
    macro_rules! instrument {
        ($(,)?) => {
            $crate::instrument!(data = "", color = 0xffffffff);
        };
        (data = $data:expr $(,)?) => {
            $crate::instrument!(data = $data, color = 0xffffffff);
        };
        (color = $color:expr $(,)?) => {
            $crate::instrument!(data = "", color = $color);
        };
        (color = $color:expr, data = $data:expr $(,)?) => {
            $crate::instrument!(data = $data, color = $color);
        };
        (data = $data:expr, color = $color:expr $(,)?) => {
            let _superluminal_scope = $crate::superluminal::InstrumentationScope::new(
                {
                    fn dummy() {}
                    $crate::superluminal::function_name(&dummy)
                },
                $data,
                $color,
            );
        };
        ($id:expr $(,)?) => {
            $crate::instrument!($id, data = "", color = 0xffffffff);
        };
        ($id:expr, data = $data:expr $(,)?) => {
            $crate::instrument!($id, data = $data, color = 0xffffffff);
        };
        ($id:expr, color = $color:expr $(,)?) => {
            $crate::instrument!($id, data = "", color = $color);
        };
        ($id:expr, color = $color:expr, data = $data:expr $(,)?) => {
            $crate::instrument!($id, data = $data, color = $color);
        };
        ($id:expr, data = $data:expr, color = $color:expr $(,)?) => {
            let _superluminal_scope =
                $crate::superluminal::InstrumentationScope::new($id, $data, $color);
        };
    }

    #[doc(hidden)]
    pub fn function_name<T>(_: &T) -> &'static str {
        let name = std::any::type_name::<T>();
        &name[..name.len() - 7]
    }

    #[doc(hidden)]
    pub struct InstrumentationScope {
        _not_send_or_sync: PhantomData<*mut ()>,
    }

    #[doc(hidden)]
    impl InstrumentationScope {
        pub fn new(id: &'static str, data: &str, color: u32) -> Self {
            unsafe {
                superluminal_perf_sys::PerformanceAPI_BeginEvent_N(
                    id.as_ptr() as *const i8,
                    id.len() as u16,
                    data.as_ptr() as *const i8,
                    data.len() as u16,
                    color,
                );
            }
            Self { _not_send_or_sync: PhantomData }
        }
    }

    #[doc(hidden)]
    impl Drop for InstrumentationScope {
        fn drop(&mut self) {
            unsafe {
                superluminal_perf_sys::PerformanceAPI_EndEvent();
            }
        }
    }
}

#[cfg(not(feature = "superluminal"))]
mod imp {
    pub fn set_current_thread_name(_name: &str) {}

    #[macro_export]
    macro_rules! instrument {
        ($($tt:tt)*) => {};
    }
}

pub use imp::*;
