#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(slice1: &[T], slice2: &[T]) -> Comparison {
    if slice1.len() == slice2.len() && slice_eq(slice1, slice2) {
        Comparison::Equal
    } else if slice1.len() < slice2.len() && is_sublist_of(slice1, slice2) {
        Comparison::Sublist
    } else if slice2.len() < slice1.len() && is_sublist_of(slice2, slice1) {
        Comparison::Superlist
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
