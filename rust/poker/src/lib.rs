mod card;
mod enums;
mod hand;
mod handref;

use crate::handref::HandRef;

use itertools::Itertools;

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which
/// happen to be equal.
pub fn winning_hands<'a>(hands: &[&'a str]) -> Option<Vec<&'a str>> {
    let mut hand_refs = parse_hand_refs(hands)?;
    hand_refs.sort_unstable();
    let groups = hand_refs.iter().rev().group_by(|&hr| hr);
    let max_hand_refs = groups.into_iter().next();
    max_hand_refs.map(|(_, group)| group.map(|hr| hr.source()).collect())
}

fn parse_hand_refs<'a>(hand_strings: &[&'a str]) -> Option<Vec<HandRef<'a>>> {
    hand_strings.into_iter().map(|&s| HandRef::from_string(s)).collect()
}
