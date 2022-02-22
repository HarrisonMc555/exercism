//! Everything in thes file is implemented in terms of required functionality.
//! You are free to use anything, if it suits you.
//! They are useful for the test framework, but the implementation is trivial.
//! We supply them to reduce work both for you and the mentors.
use crate::{Cursor, LinkedList};

impl<T> LinkedList<T> {
    /// Add an element to the back of the list.
    pub fn push_back(&mut self, element: T) {
        self.cursor_back().insert_after(element);
    }

    /// Add an element to the front of the list.
    pub fn push_front(&mut self, element: T) {
        self.cursor_front().insert_before(element);
    }

    /// Remove an element from the back of the list.
    pub fn pop_back(&mut self) -> Option<T> {
        self.cursor_back().take()
    }

    /// Remove an element from the front of the list.
    pub fn pop_front(&mut self) -> Option<T> {
        self.cursor_front().take()
    }

    /// Return a reference to the element at the front of the list.
    #[must_use]
    pub fn front(&self) -> Option<&T> {
        self.iter().next()
    }

    /// Return a reference to the element at the back of the list.
    #[must_use]
    pub fn back(&self) -> Option<&T> {
        self.iter().rev().next()
    }
}

impl<T> std::iter::FromIterator<T> for LinkedList<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut list = Self::new();
        for elem in iter {
            list.push_back(elem);
        }
        list
    }
}

// seek methods, return false if end of list is reached prematurely
impl<T> Cursor<'_, T> {
    /// Move forward by `n` nodes. Return `true` if the cursor was successfully moved forward `n` nodes. Return `false`
    /// if the cursor reached the beginning of the list.
    pub fn seek_forward(&mut self, n: usize) -> bool {
        (0..n).all(|_| self.next().is_some())
    }

    /// Move backward by `n` nodes. Return `true` if the cursor was successfully moved backward `n` nodes. Return
    /// `false` if the cursor reached the end of the list.
    pub fn seek_backward(&mut self, n: usize) -> bool {
        (0..n).all(|_| self.prev().is_some())
    }
}

// These are tests for code that must not compile. They need to be here (or in lib.rs)
// because only doctests can use `compile_fail` without additional dependencies
// and doctests are ignored inside tests/doubly-linked-list.rs.

#[allow(unused)]
#[cfg(feature = "advanced")]
/// ```compile_fail
/// use doubly_linked_list::LinkedList;
/// trait AssertSend: Send {}
/// impl<T> AssertSend for LinkedList<T> {}
/// ```
pub struct IllegalSend;

#[allow(unused)]
#[cfg(feature = "advanced")]
/// ```compile_fail
/// use doubly_linked_list::LinkedList;
/// trait AssertSync: Sync {}
/// impl<T> AssertSync for LinkedList<T> {}
/// ```
pub struct IllegalSync;
