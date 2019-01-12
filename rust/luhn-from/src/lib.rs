use std::fmt;

pub struct Luhn {
    string: String
}

impl Luhn {
    pub fn is_valid(&self) -> bool {
        luhn::is_valid(&self.string)
    }
}

// Shamelessly inspired by @coriolinus
impl<T: fmt::Display> From<T> for Luhn {
    fn from(input: T) -> Self {
        Luhn {
            string: input.to_string()
        }
    }
}
