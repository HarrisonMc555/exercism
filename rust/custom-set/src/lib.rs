use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq)]
pub struct CustomSet<T> {
    mapping: Vec<Option<Vec<T>>>,
    count: usize,
}

impl<T> CustomSet<T>
where
    T: Hash + PartialEq + Ord + Clone,
{
    const DEFAULT_CAPACITY: usize = 64;

    pub fn with_capacity(capacity: usize) -> Self {
        CustomSet {
            mapping: vec![None; capacity],
            count: 0,
        }
    }

    pub fn new(input: &[T]) -> Self {
        let capacity: usize = <CustomSet<T>>::DEFAULT_CAPACITY;
        let mut set = CustomSet::with_capacity(capacity);
        for element in input {
            set.add(element.clone());
        }
        set
    }

    pub fn contains(&self, element: &T) -> bool {
        let index = self.get_index(element);
        let list = match &self.mapping[index] {
            Some(list) => list,
            None => return false,
        };
        list.contains(element)
    }

    pub fn add(&mut self, element: T) {
        let index = self.get_index(&element);
        let list: &mut Vec<_> = match &mut self.mapping[index] {
            Some(list) => list,
            None => {
                let list = Vec::new();
                self.mapping[index] = Some(list);
                match &mut self.mapping[index] {
                    Some(list) => list,
                    None => panic!("could not successfully add into Vec"),
                }
            }
        };
        match list.binary_search(&element) {
            Ok(_) => (),
            Err(index) => {
                list.insert(index, element);
                self.count += 1;
            }
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        for elem in self.into_iter() {
            if !other.contains(elem) {
                return false;
            }
        }
        true
    }

    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        // unimplemented!(
        //     "Determine if the CustomSet struct and the other struct '{:?}' are disjoint.",
        //     other
        // );
        unimplemented!(
            "Determine if the CustomSet struct and the other struct are disjoint."
        );
    }

    pub fn intersection(&self, other: &Self) -> Self {
        // unimplemented!("Construct a new CustomSet struct that is an intersection between current struct and the other struct '{:?}'.", other);
        unimplemented!("Construct a new CustomSet struct that is an intersection between current struct and the other struct.");
    }

    pub fn difference(&self, other: &Self) -> Self {
        // unimplemented!("Construct a new CustomSet struct that is a difference between current struct and the other struct '{:?}'.", other);
        unimplemented!("Construct a new CustomSet struct that is a difference between current struct and the other struct.");
    }

    pub fn union(&self, other: &Self) -> Self {
        // unimplemented!("Construct a new CustomSet struct that is an union between current struct and the other struct '{:?}'.", other);
        unimplemented!("Construct a new CustomSet struct that is an union between current struct and the other struct.");
    }

    pub fn len(&self) -> usize {
        self.count
    }

    fn get_index(&self, element: &T) -> usize {
        CustomSet::get_hash(element) as usize % self.mapping.len()
    }

    fn get_hash(element: &T) -> u64 {
        let mut hasher = DefaultHasher::new();
        element.hash(&mut hasher);
        hasher.finish()
    }
}

impl<'a, T> IntoIterator for &'a CustomSet<T>
where
    T: Hash + PartialEq + Ord + Clone,
{
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter::new(self)
    }
}

pub struct Iter<'a, T> {
    set: &'a CustomSet<T>,
    outer_index: usize,
    inner_index: usize,
    seen: usize,
}

impl<'a, T> Iter<'a, T> {
    pub fn new(set: &'a CustomSet<T>) -> Self {
        Iter {
            set,
            outer_index: 0,
            inner_index: 0,
            seen: 0,
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T>
where
    T: Hash + PartialEq + Ord + Clone,
{
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.outer_index >= self.set.mapping.len() {
            return None;
        }
        for (i, maybe_list) in
            self.set.mapping[self.outer_index..].iter().enumerate()
        {
            if let Some(list) = maybe_list {
                if let Some(elem) = list.get(self.inner_index) {
                    self.outer_index = i;
                    self.inner_index += 1;
                    return Some(elem);
                }
            }
            self.inner_index = 0;
        }
        self.outer_index = self.set.mapping.len();
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let num_left = self.set.len() - self.seen;
        (num_left, Some(num_left))
    }
}
