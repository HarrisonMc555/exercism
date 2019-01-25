pub enum Suit {
    Heart,
    Spade,
    Diamond,
    Club,
}

pub enum Rank {
    Ace,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
}

pub struct Card {
    rank: Rank,
    suit: Suit,
}

pub struct Hand {
    cards: Vec<Card>,
}

pub struct HandRef<'a> {
    // hand: Hand,
    source: &'a str,
}

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let hand_refs = parse_hand_refs(hands);
    // hand_refs.sort();
    // hand_refs.group_by(|hand_ref| hand_ref.hand).next()
    None
}

fn parse_hand_refs<'a>(hand_strings: &[&'a str]) -> Vec<HandRef<'a>> {
    vec![]
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

impl<'a> HandRef<'a> {
    // pub fn new(hand: Hand, source: &'a str) -> Self {
    pub fn new(source: &'a str) -> Self {
        // HandRef { hand, source }
        HandRef { source }
    }
}

impl<'a> From<&'a str> for HandRef<'a> {
    fn from(source: &'a str) -> Self {
        HandRef {
            // hand: Hand::from(source),
            source
        }
    }
}

// impl<'a> Ord for HandRef<'a> {

// }
