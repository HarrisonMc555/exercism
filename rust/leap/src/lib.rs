pub fn is_leap_year(year: i32) -> bool {
    divisible_by(year, 4) && (!divisible_by(year, 100)
                              || divisible_by(year, 400))
}

fn divisible_by(number: i32, divisor: i32) -> bool {
    number % divisor == 0
}
