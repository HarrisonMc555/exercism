fn square(n: u32) -> u32 {
    n * n
}

pub fn square_of_sum(n: u32) -> u32 {
    let sum = (1..=n).sum();
    square(sum)
}

pub fn sum_of_squares(n: u32) -> u32 {
    (1..=n).map(square).sum()
}

pub fn difference(n: u32) -> u32 {
    square_of_sum(n) - sum_of_squares(n)
}
