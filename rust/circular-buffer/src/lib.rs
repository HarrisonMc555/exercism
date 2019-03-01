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

    fn is_full(&self) -> bool {
        self.reached_capacity_flag
    }

    fn is_empty(&self) -> bool {
        !self.is_full() && self.write_index == self.read_index
    }

    fn capacity(&self) -> usize {
        self.buffer.len()
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
