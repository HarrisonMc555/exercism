pub fn encrypt(input: &str) -> String {
    println!("input: \"{}\"", input);
    let normalized = normalize(input);
    println!("normalized: \"{}\"", normalized);
    let normalized_vec: Vec<_> = normalized.chars().collect();
    let rectangle = to_rectangle(&normalized_vec);
    println!("rectangle:");
    for row in &rectangle {
        println!("\t{}", row.iter().cloned().collect::<String>());
    }
    let transposed = transpose_rectangle(&rectangle);
    println!("transposed:");
    for row in transposed {
        println!("\t{}", row.iter().cloned().collect::<String>());
    }
    normalized
}

fn normalize(input: &str) -> String {
    input
        .chars()
        .filter(|c| c.is_ascii_alphabetic())
        .map(|c| c.to_ascii_lowercase())
        .collect()
}

fn to_rectangle<T>(vec: &[T]) -> Vec<Vec<T>>
    where T: Clone,
{
    let (r, c) = calculate_dimensions(vec.len());
    // let letters: Vec<_> = input.chars().collect();
    // let num_leftover = r * c - letters.len();
    let num_leftover = r * c - vec.len();
    // let extra_spaces = (0..num_leftover).map(|_| ' ');
    // let mut rectangle: Vec<Vec<char>> = letters
    //     .chunks(c)
    //     .map(|row| row.iter().cloned().collect())
    //     .collect();
    // rectangle.last_mut().map(|v| v.extend(extra_spaces));
    // rectangle
    vec
        .chunks(c)
        .map(|row| row.iter().cloned().collect())
        .collect()
    // (0..r)
    //     .map(|i| (0..c)
    //          .map(|j| at_or(&letters, i + r*j, &' '))
    //          .collect())
    //     .collect()
}

fn transpose_rectangle<T>(rectangle: &[Vec<T>]) -> Vec<Vec<T>>
where
    T: Clone,
{
    if rectangle.is_empty() {
        return Vec::new();
    }
    let new_r = rectangle.iter().map(|row| row.len()).max().unwrap();
    (0..new_r)
        .map(|i| rectangle.iter().filter_map(|row| at(row, i).map(|t| t.clone())).collect())
        .collect()
}

fn flatten_rectangle<T>(rectangle: &[Vec<T>]) -> Vec<T>
    where T: Clone,
{
    rectangle.iter().flat_map(|row| row).cloned().collect()
}

fn calculate_dimensions(size: usize) -> (usize, usize) {
    let dim = (size as f64).sqrt() as usize;
    if dim * dim >= size {
        (dim, dim)
    } else if dim * (dim + 1) >= size {
        (dim, dim + 1)
    } else {
        (dim + 1, dim + 1)
    }
}

// fn at_or<'a, T>(slice: &'a [T], index: usize, default: &'a T) -> &'a T {
//     if index >= slice.len() {
//         default
//     } else {
//         &slice[index]
//     }
// }

fn at<'a, T>(slice: &'a [T], index: usize) -> Option<&'a T> {
    if index >= slice.len() {
        None
    } else {
        Some(&slice[index])
    }
}
