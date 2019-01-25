#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Suit {
    Heart,
    Spade,
    Diamond,
    Club,
}

#[derive(Copy, Clone, Ord, Eq, PartialOrd, PartialEq, Debug)]
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
    pub fn try_from(c: char) -> Option<Self> {
        match c {
            'H' => Some(Suit::Heart),
            'S' => Some(Suit::Spade),
            'D' => Some(Suit::Diamond),
            'C' => Some(Suit::Club),
            _ => None,
        }
    }
}

impl Rank {
    pub fn try_from(s: &str) -> Option<(Self, &str)> {
        if s.is_empty() {
            return None;
        }
        let first_two_letters: Vec<_> = s.chars().take(2).collect();
        let first_letter = first_two_letters[0];
        match first_letter {
            '2' => Some((Rank::Two, &s[1..])),
            '3' => Some((Rank::Three, &s[1..])),
            '4' => Some((Rank::Four, &s[1..])),
            '5' => Some((Rank::Five, &s[1..])),
            '6' => Some((Rank::Six, &s[1..])),
            '7' => Some((Rank::Seven, &s[1..])),
            '8' => Some((Rank::Eight, &s[1..])),
            '9' => Some((Rank::Nine, &s[1..])),
            '1' => {
                if first_two_letters.len() < 2 || first_two_letters[1] != '0' {
                    None
                } else {
                    Some((Rank::Ten, &s[2..]))
                }
            }
            'J' => Some((Rank::Jack, &s[1..])),
            'Q' => Some((Rank::Queen, &s[1..])),
            'K' => Some((Rank::King, &s[1..])),
            'A' => Some((Rank::Ace, &s[1..])),
            _ => None,
        }
    }

    pub fn next_enum(&self) -> Option<Self> {
        match(self) {
            Rank::Two => Some(Rank::Three),
            Rank::Three => Some(Rank::Four),
            Rank::Four => Some(Rank::Five),
            Rank::Five => Some(Rank::Six),
            Rank::Six => Some(Rank::Seven),
            Rank::Seven => Some(Rank::Eight),
            Rank::Eight => Some(Rank::Nine),
            Rank::Nine => Some(Rank::Ten),
            Rank::Ten => Some(Rank::Jack),
            Rank::Jack => Some(Rank::Queen),
            Rank::Queen => Some(Rank::King),
            Rank::King => Some(Rank::Ace),
            Rank::Ace => None,
        }
    }

    pub fn prev_enum(&self) -> Option<Self> {
        match(self) {
            Rank::Two => None,
            Rank::Three => Some(Rank::Two),
            Rank::Four => Some(Rank::Three),
            Rank::Five => Some(Rank::Four),
            Rank::Six => Some(Rank::Five),
            Rank::Seven => Some(Rank::Six),
            Rank::Eight => Some(Rank::Seven),
            Rank::Nine => Some(Rank::Eight),
            Rank::Ten => Some(Rank::Nine),
            Rank::Jack => Some(Rank::Ten),
            Rank::Queen => Some(Rank::Jack),
            Rank::King => Some(Rank::Queen),
            Rank::Ace => Some(Rank::King),
        }
    }
}
