use std::fmt::Debug;
use std::hash::Hash;

#[derive(Debug, PartialEq)]
pub struct CustomSet<T> {
    mapping: Vec<Option<Vec<T>>>,
    count: usize,
}

impl<T> CustomSet<T>
where T: Debug + Hash + PartialEq + Clone
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
        for value in input {
            set.add(value.clone());
        }
        set
    }

    pub fn contains(&self, element: &T) -> bool {
        unimplemented!(
            "Determine if the '{:?}' element is present in the CustomSet struct.",
            element
        );
    }

    pub fn add(&mut self, element: T) {
        unimplemented!("Add the '{:?}' element to the CustomSet struct.", element);
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
}
