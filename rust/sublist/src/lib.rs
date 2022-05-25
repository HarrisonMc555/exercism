use std::cmp::Ordering;

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(slice1: &[T], slice2: &[T]) -> Comparison {
    let (smaller, larger, comparison) = match slice1.len().cmp(&slice2.len()) {
        Ordering::Equal => (slice1, slice2, Comparison::Equal),
        Ordering::Less => (slice1, slice2, Comparison::Sublist),
        Ordering::Greater => (slice2, slice1, Comparison::Superlist),
    };

    if is_sublist_of(smaller, larger) {
        comparison
    } else {
        Comparison::Unequal
    }
}

fn is_sublist_of<T: PartialEq>(sublist: &[T], superlist: &[T]) -> bool {
    if sublist.is_empty() {
        return true;
    }

    let sublen = sublist.len();
    for i in 0..=superlist.len() - sublen {
        if slice_eq(sublist, &superlist[i..i + sublen]) {
            return true;
        }
    }

    false
}

fn slice_eq<T: PartialEq>(a: &[T], b: &[T]) -> bool {
    if a.len() != b.len() {
        return false;
    }

    for i in 0..a.len() {
        if a[i] != b[i] {
            return false;
        }
    }

    true
}
