use crate::enums::Rank;
use crate::enums::Suit;
use std::cmp::Ordering;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Card {
    rank: Rank,
    suit: Suit,
}

impl Card {
    pub fn new(rank: Rank, suit: Suit) -> Self {
        Card { rank, suit }
    }

    pub fn rank(&self) -> Rank {
        self.rank
    }

    pub fn suit(&self) -> Suit {
        self.suit
    }

    pub fn from_string(string: &str) -> Option<Self> {
        let letters: Vec<_> = string.chars().collect();
        let rank_str: String =
            letters.get(..letters.len() - 1)?.iter().collect();
        let rank = Rank::from_string(&rank_str)?;
        let suit_char = letters.get(letters.len() - 1)?;
        let suit = Suit::from_char(*suit_char)?;
        Some(Card { rank, suit })
    }
}

impl Ord for Card {
    fn cmp(&self, other: &Card) -> Ordering {
        self.rank.cmp(&other.rank)
    }
}

impl PartialOrd for Card {
    fn partial_cmp(&self, other: &Card) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
