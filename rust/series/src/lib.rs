pub fn series(digits: &str, len: usize) -> Vec<String> {
    let char_vector: Vec<char> = digits.chars().collect();
    let char_windows = windows(&char_vector, len);
    char_windows.into_iter().map(|vec| join(&vec)).collect()
}

fn windows<T>(vec: &[T], len: usize) -> Vec<Vec<T>>
    where  T: Clone,
{
    if len > vec.len() {
        return Vec::new();
    }
    let num_windows = vec.len() - len + 1;
    (0..num_windows).map(|i| get_window(&vec, len, i)).collect()
}

fn get_window<T>(vec: &[T], len: usize, index: usize) -> Vec<T>
    where T: Clone,
{
    vec[index..index + len].to_vec()
}

fn join(vec: &[char]) -> String {
    vec.into_iter().collect()
}
