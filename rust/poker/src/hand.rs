use boolinator::Boolinator;
use itertools::Itertools;
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

    fn ranks(&self) -> Vec<Rank> {
        self.cards.iter().map(Card::rank).collect()
    }

    fn count_ranks(&self) -> Vec<(usize, Rank)> {
        self.ranks().iter().group_by(|&r| r).into_iter().map(|(&rank, group)|
                                                            (group.count(), rank)).collect()
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

    fn low_card(&self) -> Option<Card> {
        self.cards.first().cloned()
    }

    fn high_card(&self) -> Option<Card> {
        self.cards.last().cloned()
    }
}

impl From<&str> for Hand {
    fn from(string: &str) -> Self {
        Hand::from_string(string)
    }
}

impl HandScore {
    fn royal_flush(hand: &Hand) -> Option<HandScore> {
        (hand.all_in_a_row() &&
         hand.high_card().map(|card| card.rank().is_ace()) == Some(true))
            .as_some(HandScore::RoyalFlush)
    }

    fn straight_flush(hand: &Hand) -> Option<HandScore> {
        let high_rank = hand.high_card().map(|card| card.rank());
        (hand.all_in_a_row() &&
         high_rank.map(|rank| rank.is_ace()) == Some(false))
            .as_some(HandScore::StraightFlush(high_rank.unwrap()))
    }

    fn four_of_a_kind(hand: &Hand) -> Option<HandScore> {
//         let groups: Vec<_> = hand.cards.iter().map(|card|
//                                                    card.rank()).group_by(|x| x).collect();
//         groups.find(|())
// into_iter().map(|(_, group)| group.count()).collect();
//         (groups.len() == 2 && groups.contains(&4)).as_some(HandScore::FourOfAKind)
        None
    }
}
