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
    // Parse
    let mut hand_refs = parse_hand_refs(hands)?;
    // Sort high-to-low
    hand_refs.sort_unstable_by_key(|hand_ref| hand_ref.score());
    hand_refs.reverse();
    // Collect all that have equal highest scores
    let groups = hand_refs.iter().group_by(|&hr| hr.score());
    let (_, highest_scoring_hand_refs) = groups.into_iter().next()?;
    // Extract the string references
    let strings = highest_scoring_hand_refs.map(|hr| hr.source()).collect();
    Some(strings)
}

fn parse_hand_refs<'a>(hand_strings: &[&'a str]) -> Option<Vec<HandRef<'a>>> {
    hand_strings
        .into_iter()
        .map(|&s| HandRef::from_string(s))
        .collect()
}
