mod card;
mod enums;
mod hand;
mod handref;

use crate::hand::Hand;
use crate::handref::HandRef;

use itertools::Itertools;

fn main() {
    winning_hands(&[
            "4D 5S 6S 8D 3C",
            "2S 4C 7S 9H 10H",
            "3S 4S 5D 6H JH",
            "3H 4H 5C 6C JD",
        ]);
    let h1 = Hand::from_string("3S 4S 5D 6H JH").unwrap();
    let h2 = Hand::from_string("3H 4H 5C 6C JD").unwrap();
    // println!("{} {:?} {}", h1, h1.cmp(&h2), h2);
    // println!("{} -> {:?}", h1, h1.score());
    // println!("{} -> {:?}", h2, h2.score());
}

pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    // println!("winning_hands.A");
    let mut hand_refs = parse_hand_refs(hands)?;
    // println!("winning_hands.B");
    hand_refs.sort_unstable_by_key(|hand_ref| hand_ref.score());
    // println!("winning_hands.C");
    hand_refs.reverse(); // High-to-low
    // println!("winning_hands.D");
    let groups = hand_refs.iter().group_by(|&hr| hr.score());
    // println!("winning_hands.E");
    let (_, highest_scoring_hand_refs) = groups.into_iter().next()?;
    // println!("winning_hands.F");
    let strings = highest_scoring_hand_refs.map(|hr| hr.source()).collect();
    // println!("winning_hands.G");
    Some(strings)
}

fn parse_hand_refs<'a>(hand_strings: &[&'a str]) -> Option<Vec<HandRef<'a>>> {
    hand_strings
        .into_iter()
        .map(|&s| HandRef::from_string(s))
        .collect()
}
