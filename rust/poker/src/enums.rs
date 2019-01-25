#[derive(Eq, PartialEq, Debug)]
pub enum Suit {
    Heart,
    Spade,
    Diamond,
    Club,
}

#[derive(Ord, Eq, PartialOrd, PartialEq, Debug)]
pub enum Rank {
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
    Ace,
}

impl Suit {
    fn try_from(c: char) -> Option<Self> {
        match c {
            'H' => Some(Suit::Heart),
            'S' => Some(Suit::Spade),
            'D' => Some(Suit::Diamond),
            'C' => Some(Suit::Club),
            _ => None,
        }
    }
}
