//! Scoped thread-local storage
//!
//! This module provides the ability to generate *scoped* thread-local
//! variables. In this sense, scoped indicates that thread local storage
//! actually stores a reference to a value, and this reference is only placed
//! in storage for a scoped amount of time.
//!
//! There are no restrictions on what types can be placed into a scoped
//! variable, but all scoped variables are initialized to the equivalent of
//! null. Scoped thread local storage is useful when a value is present for a known
//! period of time and it is not required to relinquish ownership of the
//! contents.
//!
//! # Examples
//!
//! ```
//! #[macro_use]
//! extern crate scoped_tls;
//!
//! scoped_thread_local!(static FOO: u32);
//!
//! # fn main() {
//! // Initially each scoped slot is empty.
//! assert!(!FOO.is_set());
//!
//! // When inserting a value, the value is only in place for the duration
//! // of the closure specified.
//! FOO.set(&1, || {
//!     FOO.with(|slot| {
//!         assert_eq!(*slot, 1);
//!     });
//! });
//! # }
//! ```

#![deny(missing_docs, warnings)]

use crate::thread::LocalKey;
use std::cell::Cell;
use std::fmt::Debug;
use std::marker;

/// The macro. See the module level documentation for the description and examples.
#[macro_export]
macro_rules! scoped_thread_local {
    ($(#[$attrs:meta])* $vis:vis static $name:ident: $ty:ty) => (
        $(#[$attrs])*
        $vis static $name: $crate::scoped_tls::ScopedKey<$ty> = $crate::scoped_tls::ScopedKey {
            inner: {
                $crate::thread_local!(static FOO: ::std::cell::Cell<*const ()> = const {
                    ::std::cell::Cell::new(::std::ptr::null())
                });
                &FOO
            },
            _marker: ::std::marker::PhantomData,
        };
    )
}

/// Type representing a thread local storage key corresponding to a reference
/// to the type parameter `T`.
///
/// Keys are statically allocated and can contain a reference to an instance of
/// type `T` scoped to a particular lifetime. Keys provides two methods, `set`
/// and `with`, both of which currently use closures to control the scope of
/// their contents.
pub struct ScopedKey<T> {
    #[doc(hidden)]
    pub inner: &'static LocalKey<Cell<*const ()>>,
    #[doc(hidden)]
    pub _marker: marker::PhantomData<T>,
}

unsafe impl<T> Sync for ScopedKey<T> {}

impl<T> ScopedKey<T> {
    /// Inserts a value into this scoped thread local storage slot for a
    /// duration of a closure.
    ///
    /// While `f` is running, the value `t` will be returned by `get` unless
    /// this function is called recursively inside of `f`.
    ///
    /// Upon return, this function will restore the previous value, if any
    /// was available.
    ///
    /// # Examples
    ///
    /// ```
    /// #[macro_use]
    /// extern crate scoped_tls;
    ///
    /// scoped_thread_local!(static FOO: u32);
    ///
    /// # fn main() {
    /// FOO.set(&100, || {
    ///     let val = FOO.with(|v| *v);
    ///     assert_eq!(val, 100);
    ///
    ///     // set can be called recursively
    ///     FOO.set(&101, || {
    ///         // ...
    ///     });
    ///
    ///     // Recursive calls restore the previous value.
    ///     let val = FOO.with(|v| *v);
    ///     assert_eq!(val, 100);
    /// });
    /// # }
    /// ```
    pub fn set<F, R>(&'static self, t: &T, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        struct Reset {
            key: &'static LocalKey<Cell<*const ()>>,
            val: *const (),
        }
        impl Drop for Reset {
            fn drop(&mut self) {
                self.key.with(|c| c.set(self.val));
            }
        }
        let prev = self.inner.with(|c| {
            let prev = c.get();
            c.set(t as *const T as *const ());
            prev
        });
        let _reset = Reset {
            key: self.inner,
            val: prev,
        };
        f()
    }

    /// Gets a value out of this scoped variable.
    ///
    /// This function takes a closure which receives the value of this
    /// variable.
    ///
    /// # Panics
    ///
    /// This function will panic if `set` has not previously been called.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// #[macro_use]
    /// extern crate scoped_tls;
    ///
    /// scoped_thread_local!(static FOO: u32);
    ///
    /// # fn main() {
    /// FOO.with(|slot| {
    ///     // work with `slot`
    /// # drop(slot);
    /// });
    /// # }
    /// ```
    pub fn with<F, R>(&'static self, f: F) -> R
    where
        F: FnOnce(&T) -> R,
    {
        let val = self.inner.with(|c| c.get());
        assert!(
            !val.is_null(),
            "cannot access a scoped thread local \
                                 variable without calling `set` first"
        );
        unsafe { f(&*(val as *const T)) }
    }

    /// Test whether this TLS key has been `set` for the current thread.
    pub fn is_set(&'static self) -> bool {
        self.inner.with(|c| !c.get().is_null())
    }
}

impl<T: Debug> Debug for ScopedKey<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        f.debug_struct("ScopedKey").field("inner", &self.inner).finish()
    }
}
