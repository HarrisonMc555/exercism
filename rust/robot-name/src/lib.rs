use rand::seq::SliceRandom;
use rand::thread_rng;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref LETTERS: Vec<char> = (b'A'..=b'Z').map(char::from).collect();
    static ref DIGITS: Vec<char> = (b'0'..=b'9').map(char::from).collect();
}

#[derive(Default)]
pub struct Robot {
    name: String,
}

impl Robot {
    pub fn new() -> Self {
        Robot {
            name: Robot::create_random_name(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn reset_name(&mut self) {
        self.name = Robot::create_random_name()
    }

    fn create_random_name() -> String {
        let mut rng = thread_rng();
        let mut name = Vec::new();
        for _ in 0..2 {
            name.push(LETTERS.choose(&mut rng).unwrap());
        }
        for _ in 0..3 {
            name.push(DIGITS[..].choose(&mut rng).unwrap());
        }
        name.iter().cloned().collect()
    }
}
