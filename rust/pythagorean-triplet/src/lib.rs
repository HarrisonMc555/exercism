use std::cmp::Ordering;

const TARGET: u32 = 1000;
const MAX_VALUE: u32 = TARGET;

fn get_c_value(a: u32, b: u32) -> Option<u32> {
    // Calculate powers
    let a_2 = a.pow(2);
    let b_2 = b.pow(2);
    let c_2 = a_2 + b_2;

    // Calculate and c value
    let c = (c_2 as f64).sqrt() as u32;
    let c_2_check = c.pow(2);

    // Check if it c is the true square root of a^2 + b^2
    if c_2_check == c_2 {
        Some(c)
    } else {
        None
    }
}

pub fn find() -> Option<u32> {
    for a in 1..MAX_VALUE {
        for b in a..MAX_VALUE {
            // Calculate corresponding c value (if exists)
            let c = match get_c_value(a, b) {
                Some(c) => c,
                None => continue,
            };

            // Calculate sum
            let sum = a + b + c;

            // Check sum
            match sum.cmp(&TARGET) {
                Ordering::Less => continue,
                Ordering::Equal => return Some(a * b * c),
                Ordering::Greater => break,
            }
        }
    }

    // If you didn't find anything
    None
}
