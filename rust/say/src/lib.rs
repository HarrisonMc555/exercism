const POWER_PREFIXES: [(u32, &'static str); 6] = [
    (6, "quintillion"),
    (5, "quadrillion"),
    (4, "trillion"),
    (3, "billion"),
    (2, "million"),
    (1, "thousand"),
];

pub fn encode(n: u64) -> String {
    let mut strings: Vec<_> = POWER_PREFIXES
        .iter()
        .flat_map(|&(power, prefix)| say_power(n, power, prefix))
        .collect();
    strings.extend(say_hundreds(n % 1000));
    if strings.is_empty() {
        String::from("zero")
    } else {
        strings.join(" ")
    }
}

fn say_power(n: u64, power: u32, power_prefix: &'static str) -> Option<String> {
    let powered = 1000u64.pow(power);
    let this_power = n / powered % 1000;
    say_hundreds(this_power).map(|s| s + " " + power_prefix)
}

fn say_hundreds(n: u64) -> Option<String> {
    let (hundreds, rem) = (n / 100, n % 100);
    let hundreds_string = say_digit(hundreds).map(|s| s + " hundred");
    let tens_string = say_tens(rem);
    either_or_combine(hundreds_string, tens_string, |a, b| a + " " + &b)
}

fn say_tens(n: u64) -> Option<String> {
    if n < 10 {
        return say_digit(n);
    } else if n < 20 {
        return say_teen(n);
    }
    let (tens, ones) = (n / 10, n % 10);
    let prefix = match tens {
        0 => return None,
        1 => panic!("Didn't return teen from say_tens"),
        2 => "twenty",
        3 => "thirty",
        4 => "forty",
        5 => "fifty",
        6 => "sixty",
        7 => "seventy",
        8 => "eighty",
        9 => "ninety",
        _ => panic!("non-tens parameter to say_tens"),
    };
    let prefix = Some(String::from(prefix));
    either_or_combine(prefix, say_digit(ones), |a, b| a + "-" + &b)
}

fn say_teen(n: u64) -> Option<String> {
    let s = match n {
        10 => "ten",
        11 => "eleven",
        12 => "twelve",
        13 => "thirteen",
        14 => "fourteen",
        15 => "fifteen",
        16 => "sixteen",
        17 => "seventeen",
        18 => "eighteen",
        19 => "nineteen",
        _ => panic!("non-teen parameter to say_teen"),
    };
    Some(String::from(s))
}

fn say_digit(n: u64) -> Option<String> {
    let s = match n {
        0 => None,
        1 => Some("one"),
        2 => Some("two"),
        3 => Some("three"),
        4 => Some("four"),
        5 => Some("five"),
        6 => Some("six"),
        7 => Some("seven"),
        8 => Some("eight"),
        9 => Some("nine"),
        _ => panic!("non-digit parameter to say_digit"),
    };
    s.map(|s| String::from(s))
}

fn either_or_combine<T, F>(a: Option<T>, b: Option<T>, f: F) -> Option<T>
    where F: FnOnce(T, T) -> T
{
    match (a, b) {
        (Some(a), Some(b)) => Some(f(a, b)),
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (None, None) => None,
    }
}
