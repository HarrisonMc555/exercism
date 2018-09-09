pub fn is_leap_year(year: i32) -> bool {
    let div4 = divisible_by(year, 4);
    let div100 = divisible_by(year, 100);
    let div400 = divisible_by(year, 400);
    div4 && (!div100 || div400)
}

fn divisible_by(number: i32, divisor: i32) -> bool {
    number % divisor == 0
}
