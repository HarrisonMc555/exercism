mod card;
mod enums;
mod hand;
mod handref;

// use crate::hand::Hand;
use crate::handref::HandRef;

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let hand_refs = parse_hand_refs(hands);
    // hand_refs.sort();
    // hand_refs.group_by(|hand_ref| hand_ref.hand).next()
    None
}

fn parse_hand_refs<'a>(hand_strings: &[&'a str]) -> Vec<HandRef<'a>> {
    vec![]
}
