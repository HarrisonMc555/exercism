use std::ptr;

// this module adds some functionality based on the required implementations
// here like: `LinkedList::pop_back` or `Clone for LinkedList<T>`
// You are free to use anything in it, but it's mainly for the test framework.
mod pre_implemented;

pub struct LinkedList<T> {
    head: Link<T>,
    tail: *mut Node<T>,
}

pub struct Node<T> {
    prev: *mut Node<T>,
    data: T,
    next: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

pub struct Cursor<'a, T> {
    node: *mut Node<T>,
    list: &'a mut LinkedList<T>,
}

pub enum Iter<'a, T> {
    Finished,
    Unfinished {
        front: &'a Node<T>,
        back: *const Node<T>,
    },
}

impl<T> LinkedList<T> {
    pub fn new() -> Self {
        LinkedList {
            head: None,
            tail: ptr::null_mut(),
        }
    }

    pub fn len(&self) -> usize {
        let mut link = &self.head;
        let mut length = 0;
        while let Some(node) = link {
            link = &node.next;
            length += 1;
        }
        length
    }

    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn push_back(&mut self, element: T) {
        let mut new_tail = Box::new(Node {
            prev: self.tail,
            data: element,
            next: None,
        });

        let raw_tail: *mut _ = &mut *new_tail;

        if self.tail.is_null() {
            self.head = Some(new_tail);
        } else {
            unsafe {
                (*self.tail).next = Some(new_tail);
            }
        }

        self.tail = raw_tail;
    }

    pub fn push_front(&mut self, element: T) {
        let mut new_head = Box::new(Node {
            prev: ptr::null_mut(),
            data: element,
            next: self.head.take(),
        });

        let raw_head: *mut _ = &mut *new_head;
        if let Some(old_head) = &mut new_head.next {
            old_head.prev = raw_head;
        } else {
            self.tail = raw_head;
        }
        self.head = Some(new_head);
    }

    pub fn pop_back(&mut self) -> Option<T> {
        if self.tail.is_null() {
            return None;
        }
        let before_tail = unsafe { (*self.tail).prev };
        self.tail = before_tail;
        if before_tail.is_null() {
            self.head.take()
        } else {
            unsafe { (*before_tail).next.take() }
        }
        .map(|node| node.data)
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.head.take().map(|head| {
            let head = *head;
            self.head = head.next;

            if self.head.is_none() {
                self.tail = ptr::null_mut();
            }

            head.data
        })
    }

    pub fn front(&self) -> Option<&T> {
        unimplemented!()
    }

    pub fn back(&self) -> Option<&T> {
        unimplemented!()
    }
}
// impl<T> LinkedList<T> {
//     pub fn new() -> Self {
//         LinkedList {
//             head: None,
//             tail: ptr::null_mut(),
//         }
//     }

//     pub fn len(&self) -> usize {
//         let mut length = 0;
//         let mut link = self.head;
//         while let Some(node) = link {
//             link = node.next;
//             length += 1;
//         }
//         length
//     }

//     pub fn is_empty(&self) -> bool {
//         self.head.is_none()
//     }

//     /// Return a cursor positioned on the front element
//     pub fn cursor_front(&mut self) -> Cursor<'_, T> {
//         let head: *mut Node<T> = if let Some(node) = self.head {
//             Box::into_raw(node)
//         } else {
//             ptr::null_mut()
//         };
//         Cursor {
//             node: head,
//             list: self,
//         }
//     }

//     /// Return a cursor positioned on the back element
//     pub fn cursor_back(&mut self) -> Cursor<'_, T> {
//         // let tail = if self.tail
//         Cursor {
//             node: self.tail,
//             list: self,
//         }
//     }

//     /// Return an iterator that moves from front to back
//     pub fn iter(&self) -> Iter<'_, T> {
//         unimplemented!()
//     }
// }

// the cursor is expected to act as if it is at the position of an element
// and it also has to work with and be able to insert into an empty list.
// impl<T> Cursor<'_, T> {
//     // fn new(node: *mut Node<T>) -> Cursor {
//     //     Cursor {
//     //         node,
//     //         phantom: PhantomData,
//     //     }
//     // }

//     /// Take a mutable reference to the current element
//     pub fn peek_mut(&mut self) -> Option<&mut T> {
//         if self.node.is_null() {
//             None
//         } else {
//             unsafe { Some(&mut (*self.node).data) }
//         }
//     }

//     /// Move one position forward (towards the back) and
//     /// return a reference to the new position
//     pub fn next(&mut self) -> Option<&mut T> {
//         if self.node.is_null() {
//             return None;
//         }
//         self.node = unsafe { (*self.node).next };
//         self.peek_mut()
//     }

//     /// Move one position backward (towards the front) and
//     /// return a reference to the new position
//     pub fn prev(&mut self) -> Option<&mut T> {
//         if self.node.is_null() {
//             return None;
//         }
//         self.node = unsafe { (*self.node).prev };
//         self.peek_mut()
//     }

//     /// Remove and return the element at the current position and move the cursor
//     /// to the neighboring element that's closest to the back. This can be
//     /// either the next or previous position.
//     pub fn take(&mut self) -> Option<T> {
//         if self.node.is_null() {
//             return None;
//         }

//         let node = unsafe { self.node.read() };
//         // The cursor will be left with nothing unless we reassign it later.
//         self.node = ptr::null_mut();
//         let mut prev = node.prev;
//         let mut next = node.next;

//         if prev.is_null() {
//             eprintln!("Prev is null, setting head to {:?}", next as *mut _);
//             self.list.head = next;
//         } else {
//             eprintln!(
//                 "Prev is NOT null, setting prev's next to {:?}",
//                 next as *mut _
//             );
//             unsafe {
//                 (*prev).next = next;
//             }
//             // If there is a valid node here, we'll go here.
//             self.node = prev;
//         }

//         if let Some()
//         if next.is_null() {
//             eprintln!("Next is null, setting tail to {:?}", prev);
//             self.list.tail = prev;
//         } else {
//             eprintln!("Next is NOT null, setting next's prev to {:?}", prev);
//             unsafe {
//                 (*next).prev = prev;
//             }
//             // If there is a valid node here, we'll go here. This overwrites,
//             // and thus takes precedence over prev.
//             self.node = next;
//         }

//         Some(unsafe { Box::into_raw(node.data).read() })
//     }

//     pub fn insert_after(&mut self, data: T) {
//         let next = if self.node.is_null() {
//             eprintln!("insert_after into empty list");
//             ptr::null_mut()
//         } else {
//             eprintln!("insert_after into non-empty list");
//             unsafe { (*self.node).next }
//         };
//         let mut node = Node {
//             prev: self.node,
//             data: Box::new(data),
//             next,
//         };
//         if self.node.is_null() {
//             eprintln!("insert_after into empty list, change head/tail");
//             self.list.head = &mut node;
//             self.list.tail = &mut node;
//         } else {
//             eprintln!("insert_after into non-empty list, change self.node");
//             unsafe {
//                 (*self.node).next = &mut node;
//             }
//         }
//     }

//     pub fn insert_before(&mut self, data: T) {
//         let prev = if self.node.is_null() {
//             ptr::null_mut()
//         } else {
//             unsafe { (*self.node).prev }
//         };
//         let mut node = Node {
//             prev,
//             data: Box::new(data),
//             next: self.node,
//         };
//         if self.node.is_null() {
//             self.list.head = &mut node;
//             self.list.tail = &mut node;
//         } else {
//             unsafe {
//                 (*self.node).prev = &mut node;
//             }
//         }
//     }

//     // fn link_to_pointer(link: &Link<T>) -> *mut Node<T> {
//     //     if let Some(box_node) = link {
//     //         &mut *box_node
//     //     } else {
//     //         ptr::null_mut()
//     //     }
//     // }
// }

impl<T> LinkedList<T> {
    /// Return an iterator that moves from front to back
    pub fn iter(&self) -> Iter<'_, T> {
        if let Some(node) = &self.head {
            Iter::Unfinished {
                front: &node,
                back: self.tail,
            }
        } else {
            Iter::Finished
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<&'a T> {
        let (front, &mut back) = match self {
            Iter::Unfinished { front, back } => (front, back),
            Iter::Finished => return None,
        };
        let data = &front.data;
        if let Some(next_node) = &front.next {
            if next_node.as_ref() as *const Node<T> == back {
                // Front reached back
                *self = Iter::Finished;
            } else {
                *self = Iter::Unfinished {
                    front: &next_node,
                    back: back,
                };
            }
        } else {
            *self = Iter::Finished
        }
        Some(data)
    }
}

impl<T> Default for LinkedList<T> {
    fn default() -> Self {
        LinkedList::new()
    }
}

// impl<'a, T> Location<'a, T> {
//     pub fn from_ptr(ptr: *mut Node<T>) -> Self {
//         unimplemented!()
//     }
// }

impl<T> LinkedList<T>
where
    T: std::fmt::Debug,
{
    pub fn debug_print(&self) {
        eprintln!("head = {:?}", self.head.as_ref().map(|node| &node.data));
        eprint!("tail = ");
        if self.tail.is_null() {
            eprintln!("NULL");
        } else {
            unsafe {
                eprintln!("{:?}", (*self.tail).data);
            }
        }
        eprint!("[");
        let mut link = &self.head;
        if let Some(node) = link {
            eprint!("{:?}", node.data);
            link = &node.next;
        }
        while let Some(node) = link {
            eprint!(", {:?}", node.data);
            link = &node.next;
        }
        eprintln!("]");
        // if !self.head.is_null() {
        //     eprint!("Non-null head: ");
        //     unsafe {
        //         eprint!("{:?}", (*self.head).data);
        //     }
        // } else {
        //     eprint!("Null head (empty)");
        // }
        // let mut node = if self.head.is_null() {
        //     eprint!(" -- 'next' node is null, too");
        //     ptr::null_mut()
        // } else {
        //     eprint!(" -- 'next' node is head.next");
        //     unsafe { (*self.head).next }
        // };
        // let mut counter = 0;
        // while !node.is_null() {
        //     eprint!(" -- node is non-null");
        //     unsafe {
        //         eprint!(", {:?}", (*node).data);
        //         node = (*node).next;
        //     }
        //     counter += 1;
        //     if counter > 10 {
        //         eprint!(" ...ERROR: Too many iterations");
        //         break;
        //     }
        // }
        // eprintln!("]");
    }
}
