use crate::hand::Hand;

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
            source
        }
    }
}

// impl<'a> Ord for HandRef<'a> {

// }
