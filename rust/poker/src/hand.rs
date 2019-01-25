use crate::card::Card;

#[derive(Ord, Eq, PartialOrd, PartialEq)]
pub struct Hand {
    cards: Vec<Card>,
}

impl Hand {
    pub fn new(cards: Vec<Card>) -> Self {
        let mut cards = cards;
        cards.sort_unstable();
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
