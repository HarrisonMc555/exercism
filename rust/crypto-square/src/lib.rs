pub fn encrypt(input: &str) -> String {
    if input.is_empty() {
        return String::new();
    }
    let normalized = normalize(input);
    let normalized_vec: Vec<_> = normalized.chars().collect();
    let (r, c) = calculate_dimensions(normalized_vec.len());
    let rectangle = to_rectangle(&normalized_vec, c);
    let transposed = transpose_rectangle(&rectangle);
    transposed
        .iter()
        .map(|row| format_row(row, r))
        .collect::<Vec<String>>()
        .join(" ")
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
        .map(|row| row.to_vec())
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
                .filter_map(|row| at(row, i).cloned())
                .collect()
        })
        .collect()
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

fn at<T>(slice: &[T], index: usize) -> Option<&T> {
    if index >= slice.len() {
        None
    } else {
        Some(&slice[index])
    }
}

fn format_row(chars: &[char], width: usize) -> String {
    format!("{: <1$}", chars.iter().cloned().collect::<String>(), width)
}
