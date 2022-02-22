use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ptr;

// This module adds some functionality based on the required implementations here like: `LinkedList::pop_back` or `Clone
// for LinkedList<T>` You are free to use anything in it, but it's mainly for the test framework.
mod pre_implemented;

#[derive(Debug)]
pub struct LinkedList<T> {
    head: Link<T>,
    tail: *mut Node<T>,
}

#[derive(Debug)]
struct Node<T> {
    prev: *mut Node<T>,
    data: T,
    next: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

/// Points to either a current node in a [`LinkedList`] or, in the case that the list is empty, points to nothing.
/// This can be used to walk forwards and backwards through the list (see [`next`] and [`prev`]). It can also be used
/// to access and/or modify elements of the list (see [`peek`] and [`peek_mut`]).
///
/// [`LinkedList`]: struct.LinkedList.html
/// [`next`]: struct.Cursor.html#method.next
/// [`prev`]: struct.Cursor.html#method.prev
/// [`peek`]: struct.Cursor.html#method.peek
/// [`peek_mut`]: struct.Cursor.html#method.peek_mut
pub struct Cursor<'a, T> {
    node: *mut Node<T>,
    list: &'a mut LinkedList<T>,
}

/// Provides a iterator for a [`LinkedList`].
///
/// [`LinkedList`]: struct.LinkedList.html
pub struct Iter<'a, T> {
    data: IterData<'a, T>,
}

enum IterData<'a, T> {
    Unfinished(Unfinished<'a, T>),
    Finished,
}

struct Unfinished<'a, T> {
    front: &'a Node<T>,
    back: *const Node<T>,
}

impl<T: Display> Display for LinkedList<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let mut iter = self.iter();
        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
            for value in iter {
                write!(f, ", {}", value)?;
            }
        }
        write!(f, "]")
    }
}

impl<T> LinkedList<T> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            head: None,
            tail: ptr::null_mut(),
        }
    }

    // You may be wondering why it's necessary to have is_empty() when it can easily be determined from len().
    // It's good custom to have both because len() can be expensive for some types, whereas is_empty() is almost always
    // cheap.
    // (Also ask yourself whether len() is expensive for LinkedList)
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    /// Return a cursor positioned on the front element
    pub fn cursor_front(&mut self) -> Cursor<'_, T> {
        let head: *mut Node<T> = match &mut self.head {
            None => ptr::null_mut(),
            Some(node) => &mut **node,
        };
        Cursor {
            node: head,
            list: self,
        }
    }

    /// Return a cursor positioned on the back element
    pub fn cursor_back(&mut self) -> Cursor<'_, T> {
        Cursor {
            node: self.tail,
            list: self,
        }
    }

    /// Return an iterator that moves from front to back
    #[must_use]
    pub fn iter(&self) -> Iter<'_, T> {
        match &self.head {
            None => Iter::finished(),
            Some(head) => Iter::unfinished(head, self.tail),
        }
    }
}

// The cursor is expected to act as if it is at the position of an element and it also has to work with and be able to
// insert into an empty list.
impl<T> Cursor<'_, T> {
    /// Take an immutable reference to the current element
    #[must_use]
    pub fn peek(&self) -> Option<&T> {
        if self.node.is_null() {
            None
        } else {
            let node = unsafe { &mut *self.node };
            Some(&node.data)
        }
    }

    /// Take a mutable reference to the current element
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        if self.node.is_null() {
            None
        } else {
            let node = unsafe { &mut *self.node };
            Some(&mut node.data)
        }
    }

    /// Move one position forward (towards the back) and return a reference to the new position
    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn next(&mut self) -> Option<&mut T> {
        #[cfg(verbose)]
        {
            println!("Before next");
            println!("\tList: {:?}", self.list);
            println!("\tLRev: {}", self.list.reversed_str());
            println!("\tHead: {}", self.list.head_str());
            println!("\tTail: {}", self.list.tail_str());
            println!("\t{}", self.list.head_tail_str());
            println!("\tNode: {}", self.node_str());
        }
        if self.node.is_null() {
            return None;
        }
        let cur_node: &mut _ = unsafe { &mut *self.node };
        match &mut cur_node.next {
            Some(next_node) => {
                self.node = &mut **next_node;
                Some(&mut next_node.data)
            }
            None => {
                self.node = ptr::null_mut();
                None
            }
        }
    }

    /// Move one position backward (towards the front) and return a reference to the new position
    pub fn prev(&mut self) -> Option<&mut T> {
        #[cfg(verbose)]
        {
            println!("Before prev");
            println!("\tList: {:?}", self.list);
            println!("\tLRev: {}", self.list.reversed_str());
            println!("\tHead: {}", self.list.head_str());
            println!("\tTail: {}", self.list.tail_str());
            println!("\t{}", self.list.head_tail_str());
            println!("\tNode: {}", self.node_str());
        }
        if self.node.is_null() {
            return None;
        }
        let cur_node: &mut _ = unsafe { &mut *self.node };
        let prev = cur_node.prev;
        if prev.is_null() {
            self.node = ptr::null_mut();
            None
        } else {
            let prev = unsafe { &mut *prev };
            self.node = prev;
            Some(&mut prev.data)
        }
    }

    /// Remove and return the element at the current position and move the cursor to the neighboring element that's
    /// closest to the back. This can be either the next or previous position.
    ///
    /// # Panics
    /// If the invariants of the [`LinkedList`] are not upheld.
    ///
    /// [`LinkedList`]: struct.LinkedList.html
    pub fn take(&mut self) -> Option<T> {
        #[cfg(verbose)]
        {
            println!("Before take");
            println!("\tList: {:?}", self.list);
            println!("\tLRev: {}", self.list.reversed_str());
            println!("\tHead: {}", self.list.head_str());
            println!("\tTail: {}", self.list.tail_str());
            println!("\t{}", self.list.head_tail_str());
            println!("\tNode: {}", self.node_str());
            std::io::stdout().flush();
        }
        if self.node.is_null() {
            assert!(self.list.head.is_none());
            assert!(self.list.tail.is_null());
            return None;
        }
        assert!(self.list.head.is_some());
        assert!(!self.list.tail.is_null());

        // Before:
        //              cursor
        //                 |
        //                 v
        // +---A---+   +---B---+   +---C---+
        // |   next--->|   next--->|       |
        // |       |<---prev   |<---prev   |
        // +-------+   +-------+   +-------+
        //
        // After:
        //                          cursor
        //                             |
        //                             v
        // +---A---+               +---C---+
        // |   next--------------->|       |
        // |       |<---------------prev   |
        // +-------+               +-------+
        //
        // Changes:
        // - Take ownership of B
        // - Connect A -> C
        // - Connect C -> A
        // - Connect cursor -> C (or A if C is null)
        let cur_node = unsafe { &mut *self.node };
        let prev = cur_node.prev;
        if prev.is_null() {
            // Take ownership of B
            let mut node_old_head = self.list.head.take().unwrap();
            // Connect A -> C
            self.list.head = node_old_head.next.take();
            match self.list.head.as_mut() {
                None => {
                    // Connect Cursor -> C (null)
                    self.node = ptr::null_mut();
                    // Connect C -> A (null)
                    self.list.tail = ptr::null_mut();
                }
                Some(new_head) => {
                    // Connect Cursor -> C
                    self.node = &mut **new_head;
                    // Connect C -> A (null)
                    new_head.prev = ptr::null_mut();
                }
            }
            Some((*node_old_head).data)
        } else {
            let prev = unsafe { &mut *prev };
            assert!(prev.next.is_some());
            // Take ownership of B
            let mut cur_node_owned = prev.next.take().unwrap();
            // Connect A -> C
            prev.next = cur_node_owned.next.take();
            if prev.next.is_some() {
                // Connect Cursor -> C
                self.node = &mut **(prev.next.as_mut().unwrap());
                // Connect C -> A
                prev.next.as_mut().unwrap().prev = &mut *prev;
            } else {
                // Connect Cursor -> A (C is null)
                self.node = &mut *prev;
                // Connect C -> A
                self.list.tail = &mut *prev;
            }
            Some(cur_node_owned.data)
        }
    }

    /// Create a new node and place it *after* the current location of the cursor. If the cursor points to an empty
    /// list, point the cursor to the new and only node.
    ///
    /// # Panics
    /// If the invariants of the [`LinkedList`] are not upheld.
    ///
    /// [`LinkedList`]: struct.LinkedList.html
    pub fn insert_after(&mut self, element: T) {
        #[cfg(verbose)]
        {
            println!("Before calling insert_after with {:?}...", element);
            println!("\tList: {:?}", self.list);
            println!("\tLRev: {}", self.list.reversed_str());
            println!("\tHead: {}", self.list.head_str());
            println!("\tTail: {}", self.list.tail_str());
            println!("\t{}", self.list.head_tail_str());
            println!("\tNode: {}", self.node_str());
            std::io::stdout().flush();
        }
        if self.node.is_null() {
            // Before:
            //              cursor
            //                 |
            //                 v
            //                null
            //
            // After:
            //              cursor
            //                 |
            //                 v
            //             +-------+
            //      head-->|   next---> None
            //      null<---prev   |<---tail
            //             +-------+
            //
            // Changes:
            // - Create node
            // - Connect head -> node
            // - Connect tail -> node
            // - Connect cursor -> node
            assert!(self.list.head.is_none());
            assert!(self.list.tail.is_null());
            let mut new_node = Box::new(Node {
                prev: ptr::null_mut(),
                data: element,
                next: None,
            });
            self.list.tail = &mut *new_node;
            self.node = &mut *new_node;
            self.list.head = Some(new_node);
        } else {
            // Before:
            //  cursor
            //     |
            //     v
            // +---A---+   +---C---+
            // |   next--->|       |
            // |       |<---prev   |
            // +-------+   +-------+
            //
            // After:
            //  cursor
            //     |
            //     v
            // +---A---+   +---B---+   +---C---+
            // |   next--->|   next--->|       |
            // |       |<---prev   |<---prev   |
            // +-------+   +-------+   +-------+
            //
            // Changes:
            // - Create node B
            // - Connect A -> B
            // - Connect B -> C
            // - Connect C -> B
            // - Connect B -> A
            assert!(self.list.head.is_some());
            assert!(!self.list.tail.is_null());
            let cur_node = unsafe { &mut *self.node };
            // Create node B
            let mut new_node = Box::new(Node {
                // Connect B -> A
                prev: self.node,
                data: element,
                // Connect B -> C
                next: cur_node.next.take(),
            });
            // Connect C -> B
            *new_node
                .next
                .as_mut()
                .map_or(&mut self.list.tail, |n| &mut n.prev) = &mut *new_node;
            // Connect A -> B
            cur_node.next = Some(new_node);
        }
    }

    /// Create a new node and place it *before* the current location of the cursor. If the cursor points to an empty
    /// list, point the cursor to the new and only node.
    ///
    /// # Panics
    /// If the invariants of the [`LinkedList`] are not upheld.
    ///
    /// [`LinkedList`]: struct.LinkedList.html
    pub fn insert_before(&mut self, element: T) {
        #[cfg(verbose)]
        {
            println!("Before calling insert_before with {:?}...", element);
            println!("\tList: {:?}", self.list);
            println!("\tLRev: {}", self.list.reversed_str());
            println!("\tHead: {}", self.list.head_str());
            println!("\tTail: {}", self.list.tail_str());
            println!("\t{}", self.list.head_tail_str());
            println!("\tNode: {:?}", self.node);
            std::io::stdout().flush();
        }
        if self.node.is_null() {
            // Before:
            //              cursor
            //                 |
            //                 v
            //                null
            //
            // After:
            //              cursor
            //                 |
            //                 v
            //             +-------+
            //      head-->|   next---> None
            //      null<---prev   |<---tail
            //             +-------+
            //
            // Changes:
            // - Create node
            // - Connect head -> node
            // - Connect tail -> node
            // - Connect cursor -> node
            assert!(self.list.head.is_none());
            assert!(self.list.tail.is_null());
            let mut new_node = Box::new(Node {
                prev: ptr::null_mut(),
                data: element,
                next: None,
            });
            self.list.tail = &mut *new_node;
            self.node = &mut *new_node;
            self.list.head = Some(new_node);
        } else {
            // Before:
            //              cursor
            //                 |
            //                 v
            // +---A---+   +---C---+
            // |   next--->|       |
            // |       |<---prev   |
            // +-------+   +-------+
            //
            // After:
            //                          cursor
            //                             |
            //                             v
            // +---A---+   +---B---+   +---C---+
            // |   next--->|   next--->|       |
            // |       |<---prev   |<---prev   |
            // +-------+   +-------+   +-------+
            //
            // Changes:
            // - Create node B
            // - Connect A -> B
            // - Connect B -> C
            // - Connect C -> B
            // - Connect B -> A
            assert!(self.list.head.is_some());
            assert!(!self.list.tail.is_null());
            let cur_node = unsafe { &mut *self.node };
            let prev = cur_node.prev;
            if prev.is_null() {
                let old_head = self.list.head.take().unwrap();
                assert_eq!(&*old_head as *const _, self.node);
                // Create node B
                let mut new_node = Box::new(Node {
                    // Connect B -> A
                    prev: ptr::null_mut(),
                    data: element,
                    // Connect B -> C
                    next: Some(old_head),
                });
                // Connect C -> B
                new_node.next.as_mut().unwrap().prev = &mut *new_node;
                // Connect A -> B
                self.list.head = Some(new_node);
            } else {
                let prev = unsafe { &mut *prev };
                let cur_node_owned = prev.next.take();
                // Create node B
                let mut node = Box::new(Node {
                    // Connect B -> A
                    prev,
                    data: element,
                    // Connect B -> C
                    next: cur_node_owned,
                });
                // Connect C -> B
                cur_node.prev = &mut *node;
                // Connect A -> B
                prev.next = Some(node);
            }
        }
    }
}

impl<T: Debug> Cursor<'_, T> {
    #[must_use]
    pub fn node_str(&self) -> String {
        if self.node.is_null() {
            format!("{:p} = NULL", self.node)
        } else {
            format!("{:p} = {:?}", self.node, unsafe { &*self.node })
        }
    }
}

impl<T> Drop for LinkedList<T> {
    fn drop(&mut self) {
        while !self.is_empty() {
            self.pop_back();
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        let unfinished = match &self.data {
            IterData::Finished => return None,
            IterData::Unfinished(unfinished) => unfinished,
        };
        let value = &unfinished.front.data;
        *self = match &unfinished.front.next {
            _ if ptr::eq(unfinished.front, unfinished.back) => Iter::finished(),
            None => Iter::finished(),
            Some(node) => Iter::unfinished(node, unfinished.back),
        };
        Some(value)
    }
}

impl<'a, T> DoubleEndedIterator for Iter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let unfinished = match &self.data {
            IterData::Finished => return None,
            IterData::Unfinished(unfinished) => unfinished,
        };
        let back = unsafe { &*unfinished.back };
        let value = &back.data;
        *self = if back.prev.is_null() || ptr::eq(unfinished.front, unfinished.back) {
            Iter::finished()
        } else {
            Iter::unfinished(unfinished.front, back.prev)
        };
        Some(value)
    }
}

impl<T> Default for LinkedList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T> Iter<'a, T> {
    fn finished() -> Self {
        Iter {
            data: IterData::Finished,
        }
    }

    fn unfinished(front: &'a Node<T>, back: *const Node<T>) -> Self {
        Iter {
            data: IterData::Unfinished(Unfinished {
                front,
                back,
            })
        }
    }
}

impl<T: Debug> LinkedList<T> {
    #[must_use]
    pub fn head_tail_str(&mut self) -> String {
        let head_ptr: *mut _ = match &mut self.head {
            None => ptr::null_mut(),
            Some(head) => &mut **head,
        };
        let tail_ptr: *mut _ = self.tail;
        if head_ptr == tail_ptr {
            format!("Head: {:p}, tail: {:p} (same)", head_ptr, tail_ptr)
        } else {
            format!("Head: {:p}, tail: {:p} (different)", head_ptr, tail_ptr)
        }
    }

    #[must_use]
    pub fn reversed_str(&self) -> String {
        let mut result = String::new();
        result.push('[');
        let mut iter = self.iter();
        if let Some(first) = iter.next_back() {
            result += &format!("{:?}", first);
            for value in iter.rev() {
                result += &format!(", {:?}", value);
            }
        }
        result.push(']');
        result
    }

    #[must_use]
    pub fn head_str(&self) -> String {
        let ptr = match &self.head {
            None => ptr::null(),
            Some(head) => &**head,
        };
        let content_str = self
            .head
            .as_ref()
            .map_or_else(|| "None".to_string(), |n| format!("{:?}", n));
        format!("{:p} = {}", ptr, content_str)
    }

    #[must_use]
    pub fn tail_str(&self) -> String {
        if self.tail.is_null() {
            format!("{:p} = NULL", self.tail)
        } else {
            format!("{:p} = {:?}", self.tail, unsafe { &*self.tail })
        }
    }
}
