pub fn encrypt(input: &str) -> String {
    if input.is_empty() {
        return String::new();
    }
    println!("input: \"{}\"", input);
    let normalized = normalize(input);
    println!("normalized: \"{}\"", normalized);
    let normalized_vec: Vec<_> = normalized.chars().collect();
    let (r, c) = calculate_dimensions(normalized_vec.len());
    println!("r, c = {}, {}", r, c);
    let rectangle = to_rectangle(&normalized_vec, c);
    println!("rectangle:");
    for row in &rectangle {
        println!("\t{}", row.iter().cloned().collect::<String>());
    }
    let transposed = transpose_rectangle(&rectangle);
    println!("transposed:");
    for row in &transposed {
        println!("\t\"{: <1$}\"", row.iter().cloned().collect::<String>(), r);
    }
    
    let flattened: String = transposed
        .iter()
        .map(|row| format!("{: <1$}", row.iter().cloned().collect::<String>(), r))
        .collect::<Vec<String>>()
        .join(" ");
        // flatten_rectangle(&transposed).iter().cloned().collect();
    println!("flattened: \"{}\"", flattened);
    flattened
}

fn normalize(input: &str) -> String {
    input
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .map(|c| c.to_ascii_lowercase())
        .collect()
}

fn to_rectangle<T>(vec: &[T], num_cols: usize) -> Vec<Vec<T>>
where
    T: Clone,
{
    vec.chunks(num_cols)
        .map(|row| row.iter().cloned().collect())
        .collect()
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
        .map(|i| {
            rectangle
                .iter()
                .filter_map(|row| at(row, i).map(|t| t.clone()))
                .collect()
        }).collect()
}

// fn flatten_rectangle<T>(rectangle: &[Vec<T>], join: T) -> Vec<T>
// where
//     T: Clone,
// {
//     if rectangle.is_empty() {
//         Vec::new();
//     }
//     let len = rectangle[0].len();
//     rectangle
//         .iter()
//         .map(|row| format!("{: >1$}", len, row.iter().cloned().collect()))
//         .collect()
//         .join(join)
// }

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

fn at<'a, T>(slice: &'a [T], index: usize) -> Option<&'a T> {
    if index >= slice.len() {
        None
    } else {
        Some(&slice[index])
    }
}
