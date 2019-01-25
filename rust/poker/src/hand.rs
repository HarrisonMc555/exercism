use crate::enums::Rank;
use crate::card::Card;

#[derive(Ord, Eq, PartialOrd, PartialEq)]
pub struct Hand {
    cards: Vec<Card>,
}

enum HandScore {
    HighCard(Rank),
    Pair(Rank),
    TwoPair(Rank, Rank), // High, low
    ThreeOfAKind(Rank),
    Straight(Rank), // Highest card
    Flush,
    FullHouse(Rank, Rank), // High, low
    FourOfAKind(Rank),
    StraightFlush(Rank), // Highest card
    RoyalFlush,
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

    fn get_score(&self) -> HandScore {
        HandScore::RoyalFlush
    }

    fn all_in_a_row(&self) -> bool {
        if self.cards.is_empty() {
            return true;
        }
        let mut cur = self.cards[0].rank();
        for next in self.cards.iter().map(|card| card.rank()) {
            if cur.next_enum() != Some(next) {
                return false;
            }
            cur = next;
        }
        true
    }
}

impl From<&str> for Hand {
    fn from(string: &str) -> Self {
        Hand::from_string(string)
    }
}

impl HandScore {
    fn royal_flush(hand: &Hand) -> Option<HandScore> {
        None
    }
}
