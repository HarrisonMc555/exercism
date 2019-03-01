use std::mem;

pub struct CircularBuffer<T: Default> {
    buffer: Vec<T>,
    write_index: usize,
    read_index: usize,
    reached_capacity_flag: bool,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    EmptyBuffer,
    FullBuffer,
}

impl<T: Default> CircularBuffer<T> {
    pub fn new(capacity: usize) -> Self {
        let buffer = (0..capacity).map(|_| T::default()).collect();
        CircularBuffer {
            buffer,
            write_index: 0,
            read_index: 0,
            reached_capacity_flag: false,
        }
    }

    pub fn write(&mut self, element: T) -> Result<(), Error> {
        if self.is_full() {
            return Err(Error::FullBuffer);
        }
        self.overwrite(element);
        Ok(())
    }

    pub fn read(&mut self) -> Result<T, Error> {
        if self.is_empty() {
            return Err(Error::EmptyBuffer);
        }
        let element =
            mem::replace(&mut self.buffer[self.read_index], T::default());
        self.increment_read_index();
        self.reached_capacity_flag = false;
        Ok(element)
    }

    pub fn clear(&mut self) {
        for element in self.buffer.iter_mut() {
            // Drop all elements
            mem::replace(element, T::default());
        }
        self.write_index = 0;
        self.read_index = 0;
        self.reached_capacity_flag = false;
    }

    pub fn overwrite(&mut self, element: T) {
        if self.is_full() {
            self.increment_read_index();
        }
        self.buffer[self.write_index] = element;
        self.increment_write_index();
        self.reached_capacity_flag = self.write_index == self.read_index;
    }

    pub fn capacity(&self) -> usize {
        self.buffer.len()
    }

    pub fn len(&self) -> usize {
        if self.is_full() {
            return self.capacity();
        }
        (self.write_index + self.capacity() - self.read_index) % self.capacity()
    }

    fn is_full(&self) -> bool {
        self.reached_capacity_flag
    }

    fn is_empty(&self) -> bool {
        !self.is_full() && self.write_index == self.read_index
    }

    fn increment_write_index(&mut self) {
        self.write_index = self.increment_index(self.write_index);
    }

    fn increment_read_index(&mut self) {
        self.read_index = self.increment_index(self.read_index);
    }

    fn increment_index(&self, index: usize) -> usize {
        (index + 1) % self.capacity()
    }
}

/* IntoIterator (owned) */
impl<T: Default> IntoIterator for CircularBuffer<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { circular_buffer: self }
    }
}

pub struct IntoIter<T: Default> {
    circular_buffer: CircularBuffer<T>,
}

impl<T: Default> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.circular_buffer.read().ok()
    }
}


// /* Intoiterator (reference) */
// impl<'a, T: Default> IntoIterator for &'a CircularBuffer<T> {
//     type Item = &'a T;
//     type IntoIter = IntoIterRef<'a, T>;

//     fn into_iter(self) -> Self::IntoIter {
//         IntoIterRef::new(self)
//     }
// }

// pub struct IntoIterRef<'a, T: Default> {
//     circular_buffer: &'a CircularBuffer<T>,
//     index: usize,
//     is_finished: bool,
// }

// impl<'a, T: Default> IntoIterRef<'a, T> {
//     pub fn new(circular_buffer: &'a CircularBuffer<T>) -> Self {
//         IntoIterRef {
//             circular_buffer: circular_buffer,
//             index: circular_buffer.read_index,
//             is_finished: circular_buffer.is_full(),
//         }
//     }
// }

// impl<'a, T: Default> Iterator for IntoIterRef<'a, T> {
//     type Item = &'a T;

//     fn next(&mut self) -> Option<Self::Item> {
//         if self.is_finished {
//             return None;
//         }
//         let element = &self.circular_buffer.buffer[self.index];
//         self.circular_buffer.increment_index(self.index);
//         self.is_finished = self.index == self.circular_buffer.write_index;
//         Some(element)
//     }
// }
