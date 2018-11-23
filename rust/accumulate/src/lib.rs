pub fn map<T, U, F>(input: Vec<T>, mut f: F) -> Vec<U>
    where F: FnMut(T) -> U
{
    let mut result = Vec::with_capacity(input.len());
    for value in input {
        result.push(f(value));
    }
    result
}
