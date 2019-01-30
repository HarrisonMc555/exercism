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

    #[allow(dead_code)]
    pub fn circular_next_enum(&self) -> Self {
        match self {
            Rank::Two => Rank::Three,
            Rank::Three => Rank::Four,
            Rank::Four => Rank::Five,
            Rank::Five => Rank::Six,
            Rank::Six => Rank::Seven,
            Rank::Seven => Rank::Eight,
            Rank::Eight => Rank::Nine,
            Rank::Nine => Rank::Ten,
            Rank::Ten => Rank::Jack,
            Rank::Jack => Rank::Queen,
            Rank::Queen => Rank::King,
            Rank::King => Rank::Ace,
            Rank::Ace => Rank::Two,
        }
    }

    pub fn circular_prev_enum(&self) -> Self {
        match self {
            Rank::Two => Rank::Ace,
            Rank::Three => Rank::Two,
            Rank::Four => Rank::Three,
            Rank::Five => Rank::Four,
            Rank::Six => Rank::Five,
            Rank::Seven => Rank::Six,
            Rank::Eight => Rank::Seven,
            Rank::Nine => Rank::Eight,
            Rank::Ten => Rank::Nine,
            Rank::Jack => Rank::Ten,
            Rank::Queen => Rank::Jack,
            Rank::King => Rank::Queen,
            Rank::Ace => Rank::King,
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
