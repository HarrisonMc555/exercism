use crate::hand::Hand;
use std::cmp::Ordering;

#[derive(Debug, Eq)]
pub struct HandRef<'a> {
    hand: Hand,
    source: &'a str,
}

impl<'a> HandRef<'a> {
    pub fn _new(hand: Hand, source: &'a str) -> Self {
        HandRef { hand, source }
    }

    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn from_string(source: &'a str) -> Option<Self> {
        Some(HandRef {
            hand: Hand::from_string(source)?,
            source,
        })
    }

    pub fn score(&self) -> crate::hand::HandScore {
        self.hand.score()
    }
}

impl<'a> Ord for HandRef<'a> {
    fn cmp(&self, other: &HandRef) -> Ordering {
        self.hand.cmp(&other.hand)
    }
}

impl<'a> PartialOrd for HandRef<'a> {
    fn partial_cmp(&self, other: &HandRef) -> Option<Ordering> {
        Some(self.hand.cmp(&other.hand))
    }
}

impl<'a> PartialEq for HandRef<'a> {
    fn eq(&self, other: &HandRef) -> bool {
        self.hand.eq(&other.hand)
    }
}

impl<'a> std::fmt::Display for HandRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {})", self.hand, self.source)
    }
}
