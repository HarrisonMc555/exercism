use std::fmt;

pub trait Luhn {
    fn valid_luhn(&self) -> bool;
}

impl <T: fmt::Display> Luhn for T {
    fn valid_luhn(&self) -> bool {
        luhn::is_valid(&self.to_string())
    }
}
