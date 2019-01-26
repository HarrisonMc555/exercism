use crate::card::Card;
use crate::enums::Rank;
use boolinator::Boolinator;
use itertools::Itertools;

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
        self.ranks()
            .iter()
            .group_by(|&r| r)
            .into_iter()
            .map(|(&rank, group)| (group.count(), rank))
            .collect()
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

    fn low_rank(&self) -> Option<Rank> {
        self.low_card().map(|card| card.rank())
    }

    fn high_rank(&self) -> Option<Rank> {
        self.high_card().map(|card| card.rank())
    }
}

impl From<&str> for Hand {
    fn from(string: &str) -> Self {
        Hand::from_string(string)
    }
}

impl HandScore {
    fn royal_flush(hand: &Hand) -> Option<HandScore> {
        let high_is_ace = hand.high_rank().map(|rank| rank.is_ace()) == Some(true);
        (hand.all_in_a_row() && high_is_ace).as_some(HandScore::RoyalFlush)
    }

    fn straight_flush(hand: &Hand) -> Option<HandScore> {
        let high_rank = hand.high_rank();
        let high_is_ace = high_rank.map(|rank| rank.is_ace()) == Some(true);
        (hand.all_in_a_row() && !high_is_ace).as_some(HandScore::StraightFlush(high_rank.unwrap()))
    }

    fn four_of_a_kind(hand: &Hand) -> Option<HandScore> {
        let rank_with_four = hand
            .count_ranks()
            .into_iter()
            .find(|&(count, _)| count == 4)
            .map(|(_, rank)| rank);
        rank_with_four.map(|rank| HandScore::FourOfAKind(rank))
    }

    fn full_house(hand: &Hand) -> Option<HandScore> {
        let counted_ranks = hand.count_ranks();
        counted_ranks.sort_unstable_by_key(|&(count, _)| count);
        let counts = counted_ranks.into_iter().map(|&(count, _)| count).collect();
        if counts != vec![2, 3] {
            return None
        }
        let (rank_with_two, rank_with_three) = (counts[0], counts[1]);
        HandScore::FullHouse(rank_with_three, rank_with_two)
    }
}
