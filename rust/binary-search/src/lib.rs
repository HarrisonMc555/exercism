use std::cmp::Ordering;

pub fn find(array: &[i32], key: i32) -> Option<usize> {
    if array.is_empty() {
        return None;
    }
    let mut min_index = 0;
    let mut max_index = array.len();
    while min_index <= max_index {
        let index = average(min_index, max_index);
        let value = array[index];
        match value.cmp(&key) {
            Ordering::Less => {
                if index == array.len() - 1 {
                    return None;
                }
                min_index = index + 1;
            }
            Ordering::Greater => {
                if index == 0 {
                    return None;
                }
                max_index = index - 1;
            }
            Ordering::Equal => return Some(index),
        }
    }
    None
}

fn average(x: usize, y: usize) -> usize {
    // Find average this way to avoid overflow for very, very large arrays
    let (min, max) = if x > y { (y, x) } else { (x, y) };
    min + (max - min) / 2
}
