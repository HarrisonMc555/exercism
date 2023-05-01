use std::collections::HashMap;

pub enum Category {
    Ones,
    Twos,
    Threes,
    Fours,
    Fives,
    Sixes,
    FullHouse,
    FourOfAKind,
    LittleStraight,
    BigStraight,
    Choice,
    Yacht,
}

type Die = u8;
type Dice = [Die; 5];
type Score = u8;
pub fn score(dice: Dice, category: Category) -> Score {
    match category {
        Category::Ones => number(1, dice),
        Category::Twos => number(2, dice),
        Category::Threes => number(3, dice),
        Category::Fours => number(4, dice),
        Category::Fives => number(5, dice),
        Category::Sixes => number(6, dice),
        Category::FullHouse => full_house(dice),
        Category::FourOfAKind => four_of_a_kind(dice),
        Category::LittleStraight => little_straight(dice),
        Category::BigStraight => big_straight(dice),
        Category::Choice => choice(dice),
        Category::Yacht => yacht(dice),
    }
}

fn number(die: u8, dice: Dice) -> Score {
    count(die, dice) * die
}

fn full_house(dice: Dice) -> Score {
    let counter = create_counter(dice);
    let mut counts = counter.values().copied().collect::<Vec<_>>();
    counts.sort();
    match counts.as_slice() {
        [2, 3] => dice.into_iter().sum(),
        _ => 0,
    }
}

fn four_of_a_kind(dice: Dice) -> Score {
    let counter = create_counter(dice);
    let Some(die) = counter.iter().find(|(_, count)| **count >= 4).map(|(die, _)| die) else {
        return 0;
    };
    die * 4
}

fn little_straight(dice: Dice) -> Score {
    let counter = create_counter(dice);
    if contains_all(&counter, 1..=5) {
        30
    } else {
        0
    }
}

fn big_straight(dice: Dice) -> Score {
    let counter = create_counter(dice);
    if contains_all(&counter, 2..=6) {
        30
    } else {
        0
    }
}

fn choice(dice: Dice) -> Score {
    dice.into_iter().sum()
}

fn yacht(dice: Dice) -> Score {
    let Some(die) = dice.first() else {
        return 0;
    };
    if dice.into_iter().all(|d| d == *die) {
        50
    } else {
        0
    }
}

fn count(die: u8, dice: Dice) -> u8 {
    dice.into_iter().filter(|d| *d == die).count() as u8
}

fn create_counter(dice: Dice) -> HashMap<Die, u8> {
    let mut counter = HashMap::new();
    for die in dice {
        *counter.entry(die).or_insert(0) += 1;
    }
    counter
}

fn contains_all<Range>(counter: &HashMap<Die, u8>, mut range: Range) -> bool 
where Range: Iterator<Item = Die>{
    range.all(|die| counter.contains_key(&die))
}