use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

#[derive(Debug, PartialEq)]
pub struct CustomSet<T> {
    mapping: Vec<Option<Vec<T>>>,
    count: usize,
}

impl<T> CustomSet<T>
where T: Debug + Hash + PartialEq + Ord + Clone
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
                    None => panic!("could not successfully add into Vec")
                }
            }
        };
        match list.binary_search(&element) {
            Ok(_) => (),
            Err(index) => {
                list.insert(index, element);
                self.count += 1;
            },
        }
    }

    pub fn is_subset(&self, other: &Self) -> bool {
        unimplemented!(
            "Determine if the CustomSet struct is a subset of the other '{:?}' struct.",
            other
        );
    }

    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    pub fn is_disjoint(&self, other: &Self) -> bool {
        unimplemented!(
            "Determine if the CustomSet struct and the other struct '{:?}' are disjoint.",
            other
        );
    }

    pub fn intersection(&self, other: &Self) -> Self {
        unimplemented!("Construct a new CustomSet struct that is an intersection between current struct and the other struct '{:?}'.", other);
    }

    pub fn difference(&self, other: &Self) -> Self {
        unimplemented!("Construct a new CustomSet struct that is a difference between current struct and the other struct '{:?}'.", other);
    }

    pub fn union(&self, other: &Self) -> Self {
        unimplemented!("Construct a new CustomSet struct that is an union between current struct and the other struct '{:?}'.", other);
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
