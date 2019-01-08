const BASE: u32 = 10;

pub fn is_armstrong_number(num: u32) -> bool {
    let digits = get_digits(num);
    let power = digits.len() as u32;
    let sum = raise_all_to_power(digits, power).iter().sum();
    num == sum
}

fn get_digits(num: u32) -> Vec<u8> {
    if num == 0 {
        return vec![0];
    }
    let mut num = num;
    let mut result = Vec::new();
    while num > 0 {
        let digit = (num % BASE) as u8;
        num /= BASE;
        result.push(digit)
    }
    result.reverse();
    result
}

fn raise_all_to_power(nums: Vec<u8>, power: u32) -> Vec<u32> {
    nums.iter().map(|&n| (n as u32).pow(power)).collect()
}
