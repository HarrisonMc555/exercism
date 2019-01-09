use std::fmt::{Display, Formatter, Result};

const NUM_ROMAN_SYMBOLS: usize = 13;

const ROMAN_VALUE_SYMBOL_PAIRS: [(u32, &str); NUM_ROMAN_SYMBOLS] = [
    (1000, "M"),
    (900,  "CM"),
    (500,  "D"),
    (400,  "CD"),
    (100,  "C"),
    (90,   "XC"),
    (50,   "L"),
    (40,   "XL"),
    (10,   "X"),
    (9,    "IX"),
    (5,    "V"),
    (4,    "IV"),
    (1,    "I"),
];

pub struct Roman {
    symbol_counts: [usize; NUM_ROMAN_SYMBOLS],
}

impl Display for Roman {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut result = Vec::new();
        let symbols = ROMAN_VALUE_SYMBOL_PAIRS.iter()
            .map(|&(_, symbol)| symbol);
        for (symbol, &count) in symbols.zip(self.symbol_counts.iter()) {
            result.push(multiply_string(symbol, count));
        }
        let string = result.join("");
        write!(f, "{}", string)
    }
}

impl From<u32> for Roman {
    fn from(num: u32) -> Self {
        let mut num = num;
        let mut symbol_counts = [0; NUM_ROMAN_SYMBOLS];
        for i in 0..NUM_ROMAN_SYMBOLS {
            let (roman_value, _) = ROMAN_VALUE_SYMBOL_PAIRS[i];
            symbol_counts[i] = get_count(&mut num, roman_value);
        }
        assert!(num == 0);
        Roman {
            symbol_counts,
        }
    }
}

fn get_count(num: &mut u32, multiple: u32) -> usize {
    let mut count = 0;
    while *num >= multiple {
        *num -= multiple;
        count += 1;
    }
    count
}

fn multiply_string(s: &str, num: usize) -> String {
    (0..num).map(|_| s).collect::<Vec<_>>().join("")
}
