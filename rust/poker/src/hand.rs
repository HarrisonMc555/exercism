use crate::card::Card;
use crate::enums::{Rank, Suit};
use boolinator::Boolinator;
use itertools::Itertools;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub struct Hand {
    cards: Vec<Card>,
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum HandScore {
    None, // No cards
    HighCard(Vec<Rank>),
    Pair(Rank),
    TwoPair(Rank, Rank), // High, low
    ThreeOfAKind(Rank),
    Straight(Rank),        // Highest card
    Flush(Vec<Rank>),      // Highest to lowest
    FullHouse(Rank, Rank), // High, low
    FourOfAKind(Rank),
    StraightFlush(Rank), // Highest card
    RoyalFlush,
}

impl Hand {
    pub fn new(cards: Vec<Card>) -> Self {
        let mut cards = cards;
        cards.sort_unstable();
        cards.reverse(); // High-to-low
        Hand { cards }
    }

    pub fn from_string(string: &str) -> Option<Self> {
        let card_strings = string.split_whitespace();
        let cards = card_strings
            .map(Card::from_string)
            .collect::<Option<Vec<_>>>()?;
        Some(Hand::new(cards))
    }

    pub fn score(&self) -> HandScore {
        let scoring_functions = [
            HandScore::royal_flush,
            HandScore::straight_flush,
            HandScore::four_of_a_kind,
            HandScore::full_house,
            HandScore::flush,
            HandScore::straight,
            HandScore::three_of_a_kind,
            HandScore::two_pair,
            HandScore::pair,
            HandScore::high_card,
        ];
        for scoring_function in scoring_functions.iter() {
            if let Some(hand_score) = scoring_function(self) {
                return hand_score;
            }
        }
        HandScore::None
    }

    fn suits(&self) -> Vec<Suit> {
        self.cards.iter().map(Card::suit).collect()
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
        let ranks = self.ranks();
        let mut cur = ranks[0];
        for next in ranks {
            if cur.next_enum() != Some(next) {
                return false;
            }
            cur = next;
        }
        true
    }

    fn all_same_suit(&self) -> bool {
        let suits = self.suits();
        let first_suit = match suits.iter().next() {
            Some(s) => s,
            None => return false,
        };
        suits.iter().all(|c| c == first_suit)
    }

    // fn low_card(&self) -> Option<Card> {
    //     self.cards.first().cloned()
    // }

    fn high_card(&self) -> Option<Card> {
        self.cards.last().cloned()
    }

    // fn low_rank(&self) -> Option<Rank> {
    //     Some(self.low_card()?.rank())
    // }

    fn high_rank(&self) -> Option<Rank> {
        Some(self.high_card()?.rank())
    }

    fn n_of_a_kind(&self, num: usize) -> Option<Rank> {
        self.count_ranks()
            .into_iter()
            .find(|&(count, _)| count == num)
            .map(|(_, rank)| rank)
    }

    fn all_n_of_a_kind(&self, num: usize) -> Vec<Rank> {
        self.count_ranks()
            .into_iter()
            .filter(|&(count, _)| count == num)
            .map(|(_, rank)| rank)
            .collect()
    }
}

impl HandScore {
    fn royal_flush(hand: &Hand) -> Option<HandScore> {
        let high_is_ace = hand.high_rank()?.is_ace();
        let is_royal_flush =
            hand.all_in_a_row() && hand.all_same_suit() && high_is_ace;
        is_royal_flush.as_some(HandScore::RoyalFlush)
    }

    fn straight_flush(hand: &Hand) -> Option<HandScore> {
        let high_rank = hand.high_rank()?;
        let is_straight_flush = hand.all_in_a_row() && hand.all_same_suit();
        is_straight_flush.as_some(HandScore::StraightFlush(high_rank))
    }

    fn four_of_a_kind(hand: &Hand) -> Option<HandScore> {
        let rank_with_four = hand.n_of_a_kind(4)?;
        Some(HandScore::FourOfAKind(rank_with_four))
    }

    fn full_house(hand: &Hand) -> Option<HandScore> {
        let rank_with_three = hand.n_of_a_kind(3)?;
        let rank_with_two = hand.n_of_a_kind(2)?;
        Some(HandScore::FullHouse(rank_with_three, rank_with_two))
    }

    fn flush(hand: &Hand) -> Option<HandScore> {
        let all_same_suit = hand.all_same_suit();
        // Need to list ranks highest to lowest for comparing
        let ranks = hand.ranks().into_iter().collect();
        all_same_suit.as_some(HandScore::Flush(ranks))
    }

    fn straight(hand: &Hand) -> Option<HandScore> {
        let is_straight = hand.all_in_a_row();
        is_straight.as_some(HandScore::Straight(hand.high_rank()?))
    }

    fn three_of_a_kind(hand: &Hand) -> Option<HandScore> {
        let rank_with_three = hand.n_of_a_kind(3)?;
        Some(HandScore::ThreeOfAKind(rank_with_three))
    }

    fn two_pair(hand: &Hand) -> Option<HandScore> {
        let mut ranks_with_two = hand.all_n_of_a_kind(2);
        ranks_with_two.sort_unstable();
        let high_rank = *ranks_with_two.get(1)?;
        let low_rank = *ranks_with_two.get(0)?;
        let is_two_pair = ranks_with_two.len() == 2;
        is_two_pair.as_some(HandScore::TwoPair(high_rank, low_rank))
    }

    fn pair(hand: &Hand) -> Option<HandScore> {
        let rank_with_two = hand.n_of_a_kind(2)?;
        Some(HandScore::Pair(rank_with_two))
    }

    fn high_card(hand: &Hand) -> Option<HandScore> {
        let ranks = hand.ranks();
        if ranks.is_empty() {
            return None;
        }
        Some(HandScore::HighCard(ranks))
    }
}

impl std::fmt::Display for Hand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let card_strings: Vec<_> =
            self.cards.iter().map(Card::to_string).collect();
        write!(f, "{}", card_strings.join(" "))
    }
}
