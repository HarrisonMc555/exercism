use crate::card::Card;
use crate::enums::{Rank, Suit};
use boolinator::Boolinator;
use itertools::Itertools;
use std::cmp::Ordering;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Hand {
    cards: Vec<Card>, // High-to-low
}

// Do NOT auto-derive Ord and PartialEq, we want our own rules for those. Eq
// is a no-op and PartialOrd should just use our custom Ord implementation.
#[derive(Debug, PartialOrd, Eq)]
pub struct HandScore {
    partial_score: PartialHandScore,
    remaining_hand: Hand,
}

// The order here is precedence (lowest to highest)
#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum PartialHandScore {
    None, // No cards
    HighCard(Rank),
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

    pub fn empty() -> Self {
        Hand::new(vec![])
    }

    pub fn is_empty(&self) -> bool {
        self.cards.is_empty()
    }

    const NUM_CARDS_IN_FULL_HAND: usize = 5;

    pub fn from_string(string: &str) -> Option<Self> {
        let card_strings = string.split_whitespace();
        let cards = card_strings
            .map(Card::from_string)
            .collect::<Option<Vec<_>>>()?;
        Some(Hand::new(cards))
    }

    pub fn score(&self) -> HandScore {
        // The order of this function determines which order we test them in
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
        scoring_functions
            .iter()
            .flat_map(|fun| fun(self)) // Apply functions, skipping None
            .next() // Get first one or default
            .unwrap_or_else(|| {
                HandScore::new(PartialHandScore::None, self.clone())
            })
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

    fn highest_card_when_in_a_row(&self) -> Option<Rank> {
        if !self.is_full_hand() {
            return None;
        }

        let ranks = self.ranks();
        if Hand::is_all_in_a_row(&ranks) {
            return Some(ranks[0]);
        }

        let first_at_end: Vec<Rank> =
            ranks.iter().skip(1).chain(ranks.first()).cloned().collect();
        if Hand::is_all_in_a_row(&first_at_end) {
            return Some(ranks[1]);
        }

        None
    }

    fn is_all_in_a_row(ranks: &[Rank]) -> bool {
        ranks.windows(2).all(|w| w[0].circular_prev_enum() == w[1])
    }

    fn is_all_same_suit(&self) -> bool {
        let suits = self.suits();
        let first_suit = match suits.iter().next() {
            Some(s) => s,
            None => return false,
        };
        suits.iter().all(|c| c == first_suit)
    }

    fn is_full_hand(&self) -> bool {
        self.cards.len() == Hand::NUM_CARDS_IN_FULL_HAND
    }

    fn high_card(&self) -> Option<Card> {
        self.cards.first().cloned()
    }

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

    fn remove_up_to_n_matching_rank(&self, num: usize, rank: Rank) -> Self {
        let max_matching_index = self
            .cards
            .iter()
            .enumerate()
            .filter(|(_, card)| card.rank() == rank)
            .map(|(index, _)| index)
            .take(num)
            .last()
            .unwrap_or_else(|| self.cards.len());
        let up_to_n = &self.cards[..=max_matching_index];
        let after_n = &self.cards[max_matching_index + 1..];
        let remaining_cards = up_to_n
            .iter()
            .cloned()
            .filter(|card| card.rank() != rank)
            .chain(after_n.iter().cloned())
            .collect();
        Hand::new(remaining_cards)
    }
}

impl HandScore {
    pub fn new(
        partial_score: PartialHandScore,
        remaining_hand: Hand,
    ) -> HandScore {
        HandScore {
            partial_score,
            remaining_hand,
        }
    }

    pub fn royal_flush(hand: &Hand) -> Option<HandScore> {
        let high_rank = hand.highest_card_when_in_a_row()?;
        let is_royal_flush = hand.is_all_same_suit() && high_rank.is_ace();
        is_royal_flush.as_some(HandScore::new(
            PartialHandScore::RoyalFlush,
            Hand::empty(),
        ))
    }

    pub fn straight_flush(hand: &Hand) -> Option<HandScore> {
        let high_rank = hand.highest_card_when_in_a_row()?;
        let is_straight_flush = hand.is_all_same_suit();
        is_straight_flush.as_some(HandScore::new(
            PartialHandScore::StraightFlush(high_rank),
            Hand::empty(),
        ))
    }

    pub fn four_of_a_kind(hand: &Hand) -> Option<HandScore> {
        let rank_with_four = hand.n_of_a_kind(4)?;
        let remaining_hand =
            hand.remove_up_to_n_matching_rank(4, rank_with_four);
        Some(HandScore::new(
            PartialHandScore::FourOfAKind(rank_with_four),
            remaining_hand,
        ))
    }

    pub fn full_house(hand: &Hand) -> Option<HandScore> {
        let rank_with_three = hand.n_of_a_kind(3)?;
        let rank_with_two = hand.n_of_a_kind(2)?;
        let remaining_hand = hand
            .remove_up_to_n_matching_rank(3, rank_with_three)
            .remove_up_to_n_matching_rank(2, rank_with_two);
        Some(HandScore::new(
            PartialHandScore::FullHouse(rank_with_three, rank_with_two),
            remaining_hand,
        ))
    }

    pub fn flush(hand: &Hand) -> Option<HandScore> {
        let is_flush = hand.is_all_same_suit();
        let ranks = hand.ranks();
        is_flush.as_some(HandScore::new(
            PartialHandScore::Flush(ranks),
            Hand::empty(),
        ))
    }

    pub fn straight(hand: &Hand) -> Option<HandScore> {
        let high_rank = hand.highest_card_when_in_a_row()?;
        Some(HandScore::new(
            PartialHandScore::Straight(high_rank),
            Hand::empty(),
        ))
    }

    pub fn three_of_a_kind(hand: &Hand) -> Option<HandScore> {
        let rank_with_three = hand.n_of_a_kind(3)?;
        let remaining_hand =
            hand.remove_up_to_n_matching_rank(3, rank_with_three);
        Some(HandScore::new(
            PartialHandScore::ThreeOfAKind(rank_with_three),
            remaining_hand,
        ))
    }

    pub fn two_pair(hand: &Hand) -> Option<HandScore> {
        let mut ranks_with_two = hand.all_n_of_a_kind(2);
        ranks_with_two.sort_unstable();
        let high_rank = *ranks_with_two.get(1)?;
        let low_rank = *ranks_with_two.get(0)?;
        let is_two_pair = ranks_with_two.len() == 2;
        let remaining_hand = hand
            .remove_up_to_n_matching_rank(2, high_rank)
            .remove_up_to_n_matching_rank(2, low_rank);
        is_two_pair.as_some(HandScore::new(
            PartialHandScore::TwoPair(high_rank, low_rank),
            remaining_hand,
        ))
    }

    pub fn pair(hand: &Hand) -> Option<HandScore> {
        let rank_with_two = hand.n_of_a_kind(2)?;
        let remaining_hand =
            hand.remove_up_to_n_matching_rank(2, rank_with_two);
        Some(HandScore::new(
            PartialHandScore::Pair(rank_with_two),
            remaining_hand,
        ))
    }

    pub fn high_card(hand: &Hand) -> Option<HandScore> {
        let high_rank = hand.high_rank()?;
        let remaining_hand = hand.remove_up_to_n_matching_rank(1, high_rank);
        Some(HandScore::new(
            PartialHandScore::HighCard(high_rank),
            remaining_hand,
        ))
    }
}

impl std::fmt::Display for Hand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let card_strings: Vec<_> =
            self.cards.iter().map(Card::to_string).collect();
        write!(f, "{}", card_strings.join(" "))
    }
}

impl Ord for HandScore {
    fn cmp(&self, other: &HandScore) -> Ordering {
        let self_partial = &self.partial_score;
        let other_partial = &other.partial_score;
        if self_partial != other_partial {
            return self_partial.cmp(&other_partial);
        }
        if self.remaining_hand.is_empty() || other.remaining_hand.is_empty() {
            return self.remaining_hand.cards.cmp(&other.remaining_hand.cards);
        }
        let self_next_hand_score = self.remaining_hand.score();
        let other_next_hand_score = other.remaining_hand.score();
        self_next_hand_score.cmp(&other_next_hand_score)
    }
}

impl PartialEq for HandScore {
    fn eq(&self, other: &HandScore) -> bool {
        self.cmp(&other) == Ordering::Equal
    }
}
