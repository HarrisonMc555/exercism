pub struct Allergies(u32);

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Allergen {
    Eggs,
    Peanuts,
    Shellfish,
    Strawberries,
    Tomatoes,
    Chocolate,
    Pollen,
    Cats,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Allergies(score)
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        self.0 & allergen.score() != 0
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        Allergen::list_all()
            .into_iter()
            .filter(|a| self.is_allergic_to(a))
            .collect()
    }
}

impl Allergen {
    #[rustfmt::skip]
    pub fn score(&self) -> u32 {
        match self {
            Allergen::Eggs =>         0b00000001,
            Allergen::Peanuts =>      0b00000010,
            Allergen::Shellfish =>    0b00000100,
            Allergen::Strawberries => 0b00001000,
            Allergen::Tomatoes =>     0b00010000,
            Allergen::Chocolate =>    0b00100000,
            Allergen::Pollen =>       0b01000000,
            Allergen::Cats =>         0b10000000,
        }
    }

    pub fn list_all() -> Vec<Allergen> {
        vec![
            Allergen::Eggs,
            Allergen::Peanuts,
            Allergen::Shellfish,
            Allergen::Strawberries,
            Allergen::Tomatoes,
            Allergen::Chocolate,
            Allergen::Pollen,
            Allergen::Cats,
        ]
    }
}
