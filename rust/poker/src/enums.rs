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
    pub fn from_char(c: char) -> Option<Self> {
        Some(match c {
            'H' => Suit::Heart,
            'S' => Suit::Spade,
            'D' => Suit::Diamond,
            'C' => Suit::Club,
            _ => return None,
        })
    }

    pub fn to_string(&self) -> String {
        match self {
            Suit::Heart => "H",
            Suit::Spade => "S",
            Suit::Diamond => "D",
            Suit::Club => "C",
        }.to_string()
    }

    // pub fn is_heart(&self) -> bool {
    //     self == &Suit::Heart
    // }

    // pub fn is_spade(&self) -> bool {
    //     self == &Suit::Spade
    // }

    // pub fn is_diamond(&self) -> bool {
    //     self == &Suit::Diamond
    // }

    // pub fn is_club(&self) -> bool {
    //     self == &Suit::Club
    // }
}

impl Rank {
    pub fn from_string(s: &str) -> Option<Self> {
        if s.is_empty() {
            return None;
        }
        Some(match s {
            "2" => Rank::Two,
            "3" => Rank::Three,
            "4" => Rank::Four,
            "5" => Rank::Five,
            "6" => Rank::Six,
            "7" => Rank::Seven,
            "8" => Rank::Eight,
            "9" => Rank::Nine,
            "10" => Rank::Ten,
            "J" => Rank::Jack,
            "Q" => Rank::Queen,
            "K" => Rank::King,
            "A" => Rank::Ace,
            _ => return None,
        })
    }

    pub fn next_enum(&self) -> Option<Self> {
        match self {
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

    pub fn to_string(&self) -> String {
        match self {
            Rank::Two => "2",
            Rank::Three => "3",
            Rank::Four => "4",
            Rank::Five => "5",
            Rank::Six => "6",
            Rank::Seven => "7",
            Rank::Eight => "8",
            Rank::Nine => "9",
            Rank::Ten => "10",
            Rank::Jack => "J",
            Rank::Queen => "Q",
            Rank::King => "K",
            Rank::Ace => "A",
        }.to_string()
    }

    // pub fn prev_enum(&self) -> Option<Self> {
    //     match self {
    //         Rank::Two => None,
    //         Rank::Three => Some(Rank::Two),
    //         Rank::Four => Some(Rank::Three),
    //         Rank::Five => Some(Rank::Four),
    //         Rank::Six => Some(Rank::Five),
    //         Rank::Seven => Some(Rank::Six),
    //         Rank::Eight => Some(Rank::Seven),
    //         Rank::Nine => Some(Rank::Eight),
    //         Rank::Ten => Some(Rank::Nine),
    //         Rank::Jack => Some(Rank::Ten),
    //         Rank::Queen => Some(Rank::Jack),
    //         Rank::King => Some(Rank::Queen),
    //         Rank::Ace => Some(Rank::King),
    //     }
    // }

    // pub fn is_two(&self) -> bool {
    //     self == &Rank::Two
    // }

    // pub fn is_three(&self) -> bool {
    //     self == &Rank::Three
    // }

    // pub fn is_four(&self) -> bool {
    //     self == &Rank::Four
    // }

    // pub fn is_five(&self) -> bool {
    //     self == &Rank::Five
    // }

    // pub fn is_six(&self) -> bool {
    //     self == &Rank::Six
    // }

    // pub fn is_seven(&self) -> bool {
    //     self == &Rank::Seven
    // }

    // pub fn is_eight(&self) -> bool {
    //     self == &Rank::Eight
    // }

    // pub fn is_nine(&self) -> bool {
    //     self == &Rank::Nine
    // }

    // pub fn is_ten(&self) -> bool {
    //     self == &Rank::Ten
    // }

    // pub fn is_jack(&self) -> bool {
    //     self == &Rank::Jack
    // }

    // pub fn is_queen(&self) -> bool {
    //     self == &Rank::Queen
    // }

    // pub fn is_king(&self) -> bool {
    //     self == &Rank::King
    // }

    pub fn is_ace(&self) -> bool {
        self == &Rank::Ace
    }
}

impl std::fmt::Display for Suit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl std::fmt::Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
