use crate::card::Card;

pub struct Hand {
    cards: Vec<Card>,
}

impl Hand {
    pub fn new(cards: Vec<Card>) -> Self {
        Hand { cards }
    }

    fn from_string(string: &str) -> Self {
        Hand { cards: vec![] }
    }
}

impl From<&str> for Hand {
    fn from(string: &str) -> Self {
        Hand::from_string(string)
    }
}
