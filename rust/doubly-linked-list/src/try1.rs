use std::ptr;

// this module adds some functionality based on the required implementations
// here like: `LinkedList::pop_back` or `Clone for LinkedList<T>`
// You are free to use anything in it, but it's mainly for the test framework.
mod pre_implemented;

pub struct LinkedList<T> {
    head: *mut Node<T>,
    tail: *mut Node<T>,
    len: usize,
}

struct Node<T> {
    prev: *mut Node<T>,
    elem: T,
    next: *mut Node<T>,
}

pub struct Cursor<'a, T> {
    location: Location<'a, T>,
    list: &'a mut LinkedList<T>,
}

enum Location<'a, T> {
    Empty,
    NonEmpty(&'a mut Node<T>),
}

pub struct Iter<'a, T>(std::marker::PhantomData<&'a T>);

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        LinkedList {
            head: ptr::null_mut(),
            tail: ptr::null_mut(),
            len: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Return a cursor positioned on the front element
    pub fn cursor_front(&mut self) -> Cursor<'_, T> {
        Cursor {
            location: Location::Empty,
            list: self,
        }
    }

    /// Return a cursor positioned on the back element
    pub fn cursor_back(&mut self) -> Cursor<'_, T> {
        Cursor {
            location: Location::from_ptr(dbg!(self.tail)),
            list: self,
        }
    }

    /// Return an iterator that moves from front to back
    pub fn iter(&self) -> Iter<'_, T> {
        unimplemented!()
    }
}

// the cursor is expected to act as if it is at the position of an element
// and it also has to work with and be able to insert into an empty list.
impl<T> Cursor<'_, T> {
    /// Take a mutable reference to the current element
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        match &mut self.location {
            Location::NonEmpty(node) => Some(&mut node.elem),
            _ => None,
        }
    }

    /// Move one position forward (towards the back) and
    /// return a reference to the new position
    pub fn next(&mut self) -> Option<&mut T> {
        match &mut self.location {
            Location::Empty => (),
            Location::NonEmpty(node) => {
                if !node.next.is_null() {
                    return None;
                } else {
                    unsafe { self.location = Location::NonEmpty(&mut (*node.next)) }
                }
            }
        }
        self.peek_mut()
    }

    /// Move one position backward (towards the front) and
    /// return a reference to the new position
    pub fn prev(&mut self) -> Option<&mut T> {
        match &self.location {
            Location::Empty => (),
            Location::NonEmpty(node) => {
                if node.prev.is_null() {
                    return None;
                } else {
                    unsafe { self.location = Location::NonEmpty(&mut (*node.prev)) }
                }
            }
        }
        self.peek_mut()
    }

    /// Remove and return the element at the current position and move the cursor
    /// to the neighboring element that's closest to the back. This can be
    /// either the next or previous position.
    pub fn take(&mut self) -> Option<T> {
        let mut new_node = None;
        let elem = if let Location::NonEmpty(node) = &mut self.location {
            if !node.next.is_null() {
                unsafe {
                    (*node.next).prev = node.prev;
                    new_node = Some(node.next);
                }
            }
            if !node.prev.is_null() {
                unsafe {
                    (*node.prev).next = node.next;
                    new_node = Some(node.prev);
                }
            }
            let node = unsafe { Box::from_raw(*node) };
            self.list.len -= 1;
            Some(node.elem)
        } else {
            None
        };
        if let Some(new_node) = new_node {
            self.location = Location::NonEmpty(unsafe { &mut *new_node });
        } else {
            self.location = Location::Empty;
        }
        elem
    }

    pub fn insert_after(&mut self, elem: T) {
        eprintln!("insert_after(??)");
        let (prev, next) = match &mut self.location {
            Location::Empty => (ptr::null_mut(), ptr::null_mut()),
            Location::NonEmpty(node) => {
                let n: &mut Node<T> = node;
                (n as *mut Node<T>, node.next)
            }
        };
        dbg!((prev, next));
        let new_node = Box::new(Node { prev, elem, next });
        let new_node_ptr = Box::into_raw(new_node);
        dbg!(new_node_ptr);
        match (self.list.head.is_null(), self.list.tail.is_null()) {
            (true, true) => {
                self.list.head = new_node_ptr;
                self.list.tail = new_node_ptr;
            }
            (false, false) => (),
            (true, false) => panic!("Empty is null but tail is not!"),
            (false, true) => panic!("Tail is null but head is not!"),
        }
        match &mut self.location {
            Location::NonEmpty(node) => {
                let old_next = node.next;
                if !old_next.is_null() {
                    unsafe {
                        (*old_next).prev = new_node_ptr;
                    }
                }
                node.next = new_node_ptr;
            }
            Location::Empty => {
                unsafe {
                    self.location = Location::NonEmpty(&mut (*new_node_ptr));
                }
                self.list.tail = new_node_ptr;
            }
        }
        self.list.len += 1;
        eprint!("[");
        let mut n = self.list.head;
        while !n.is_null() {
            eprint!("???, ");
            unsafe { n = (*n).next }
        }
        eprintln!("]");
    }

    pub fn insert_before(&mut self, _element: T) {
        unimplemented!()
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        unimplemented!()
    }
}

impl<T> Default for LinkedList<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T> Location<'a, T> {
    pub fn from_ptr(ptr: *mut Node<T>) -> Self {
        if ptr.is_null() {
            Location::Empty
        } else {
            unsafe { Location::NonEmpty(&mut (*ptr)) }
        }
    }
}

impl<T> LinkedList<T>
where
    T: std::fmt::Debug,
{
    pub fn debug_print(&self) {
        let mut node = self.head;
        eprint!("[");
        let mut at_head = true;
        while !node.is_null() {
            if at_head {
                at_head = false;
            } else {
                eprint!(",");
            }
            unsafe {
                eprint!("{:?}", (*node).elem);
                node = (*node).next;
            }
        }
        eprintln!("]");
    }
}
