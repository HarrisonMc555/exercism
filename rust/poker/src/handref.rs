use crate::hand::Hand;
use std::cmp::Ordering;

#[derive(Eq)]
pub struct HandRef<'a> {
    hand: Hand,
    source: &'a str,
}

impl<'a> HandRef<'a> {
    pub fn new(hand: Hand, source: &'a str) -> Self {
        HandRef { hand, source }
    }
}

impl<'a> From<&'a str> for HandRef<'a> {
    fn from(source: &'a str) -> Self {
        HandRef {
            hand: Hand::from(source),
            source,
        }
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
