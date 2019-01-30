use crate::card::Card;
use crate::enums::{Rank, Suit};
use boolinator::Boolinator;
use itertools::Itertools;
use std::cmp::Ordering;

#[derive(Debug, Clone, Ord, PartialOrd, Eq)]
pub struct Hand {
    cards: Vec<Card>,
}

#[derive(Debug, Eq)]
pub struct HandScore {
    partial_score: PartialHandScore,
    remaining_hand: Hand,
}

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
                // if self.cards.len() <= hand_score.remaining_hand.cards.len() {
                //     println!("Original hand : {}", self);
                //     println!("Remaining hand: {}", hand_score.remaining_hand);
                //     println!("hand_score: {:?}", hand_score);
                // }
                assert!(
                    self.cards.len() > hand_score.remaining_hand.cards.len()
                );
                return hand_score;
            }
        }
        HandScore::new(PartialHandScore::None, self.clone())
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

    fn is_full_hand(&self) -> bool {
        self.cards.len() == Hand::NUM_CARDS_IN_FULL_HAND
    }

    // fn low_card(&self) -> Option<Card> {
    //     self.cards.first().cloned()
    // }

    fn high_card(&self) -> Option<Card> {
        self.cards.first().cloned()
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

    fn remove_up_to_n_matching_rank(&self, num: usize, rank: Rank) -> Self {
        // println!("remove_up_to_n_matching_rank:");
        // println!("\tself: {}", self);
        // println!("\tnum: {}", num);
        // println!("\trank: {}", rank);
        let max_matching_index = self
            .cards
            .iter()
            .enumerate()
            .filter(|(_, card)| card.rank() == rank)
            .map(|(index, _)| index)
            .take(num)
            .last()
            .unwrap_or_else(|| self.cards.len());
        // println!("\tmax_matching_index: {}", max_matching_index);
        let up_to_n = &self.cards[..=max_matching_index];
        let after_n = &self.cards[max_matching_index + 1..];
        let remaining_cards = up_to_n
            .iter()
            .cloned()
            .filter(|card| card.rank() != rank)
            .chain(after_n.iter().cloned())
            .collect();
        // println!("\tremaining_cards: {:?}", remaining_cards);
        Hand::new(remaining_cards)
        // let cards = self
        //     .cards
        //     .iter()
        //     .cloned()
        //     .filter(|c| c.rank() != rank)
        //     .take(num)
        //     .collect();
        // Hand::new(cards)
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

    pub fn remaining_hand(&self) -> Hand {
        self.remaining_hand.clone()
    }

    pub fn partial_score(&self) -> PartialHandScore {
        self.partial_score.clone()
    }

    pub fn royal_flush(hand: &Hand) -> Option<HandScore> {
        if !hand.is_full_hand() {
            return None;
        }
        let high_is_ace = hand.high_rank()?.is_ace();
        let is_royal_flush =
            hand.all_in_a_row() && hand.all_same_suit() && high_is_ace;
        is_royal_flush.as_some(HandScore::new(
            PartialHandScore::RoyalFlush,
            Hand::empty(),
        ))
    }

    pub fn straight_flush(hand: &Hand) -> Option<HandScore> {
        if !hand.is_full_hand() {
            return None;
        }
        let high_rank = hand.high_rank()?;
        let is_straight_flush = hand.all_in_a_row() && hand.all_same_suit();
        is_straight_flush.as_some(HandScore::new(
            PartialHandScore::StraightFlush(high_rank),
            Hand::empty(),
        ))
    }

    pub fn four_of_a_kind(hand: &Hand) -> Option<HandScore> {
        let rank_with_four = hand.n_of_a_kind(4)?;
        let remaining_hand =
            // hand.remove_up_to_n_matching(|card| card.rank() == rank_with_four, 4);
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
        if !hand.is_full_hand() {
            return None;
        }
        let all_same_suit = hand.all_same_suit();
        let ranks = hand.ranks();
        all_same_suit.as_some(HandScore::new(
            PartialHandScore::Flush(ranks),
            Hand::empty(),
        ))
    }

    pub fn straight(hand: &Hand) -> Option<HandScore> {
        if !hand.is_full_hand() {
            return None;
        }
        let is_straight = hand.all_in_a_row();
        is_straight.as_some(HandScore::new(
            PartialHandScore::Straight(hand.high_rank()?),
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
        // println!(
        //     "TwoPair({}, {}), remaining = {:?} (original = {})",
        //     high_rank, low_rank, remaining_hand, hand
        // );
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
        // let ranks = hand.ranks();
        // if ranks.is_empty() {
        //     return None;
        // }
        // Some(HandScore::new(
        //     PartialHandScore::HighCard(ranks),
        //     Hand::empty(),
        // ))
    }
}

impl std::fmt::Display for Hand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let card_strings: Vec<_> =
            self.cards.iter().map(Card::to_string).collect();
        write!(f, "{}", card_strings.join(" "))
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Hand) -> bool {
        self.cmp(&other) == Ordering::Equal
    }
}

impl Ord for HandScore {
    fn cmp(&self, other: &HandScore) -> Ordering {
        let self_partial = &self.partial_score;
        let other_partial = &other.partial_score;
        if self_partial != other_partial {
            println!(
                "HandScore::cmp A -> {:?} {:?} {:?}\n",
                self_partial,
                self_partial.cmp(&other_partial),
                other_partial
            );
            return self_partial.cmp(&other_partial);
        }
        println!("HandScore::cmp B ->\n\t{:?}\n\t==\n\t{:?}", self, other);
        if self.remaining_hand.is_empty() || other.remaining_hand.is_empty() {
            println!("\t...but one is empty");
            println!(
                "\t...answer is {:?}\n",
                self.remaining_hand.cards.cmp(&other.remaining_hand.cards)
            );
            return self.remaining_hand.cards.cmp(&other.remaining_hand.cards);
        }
        let self_next_hand_score = self.remaining_hand.score();
        let other_next_hand_score = other.remaining_hand.score();
        println!(
            "\nHandScore::cmp C ->\n\t{:?}\n\t??\n\t{:?}...\n",
            self_next_hand_score, other_next_hand_score
        );
        self_next_hand_score.cmp(&other_next_hand_score)
    }
}

impl PartialOrd for HandScore {
    fn partial_cmp(&self, other: &HandScore) -> Option<Ordering> {
        return Some(self.cmp(other));
    }
}

impl PartialEq for HandScore {
    fn eq(&self, other: &HandScore) -> bool {
        return self.cmp(&other) == Ordering::Equal
    }
}
