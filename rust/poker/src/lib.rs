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

    for window in hand_refs.windows(2) {
        let hr1 = &window[0];
        let score1 = hr1.score();
        let hr2 = &window[1];
        let cmp = hr1.cmp(hr2);
        let score2 = hr2.score();
        println!("{} ({:?}) {:?} ({:?}) {}", hr1, score1, cmp, score2, hr2);
    }

    let groups = hand_refs.iter().rev().group_by(|&hr| hr);
    for (_, group) in &groups {
        println!("group:");
        for handref in group {
            println!("\t{} -> {:?}", handref, handref.score());
        }
        // println!("group: {:?}\n", group.collect::<Vec<_>>());
    }
    // println!(
    //     "groups: {:?}",
    //     groups
    //         .into_iter()
    //         .map(|(_, group)| group.collect::<Vec<_>>())
    //         .collect::<Vec<_>>()
    // );
    let groups = hand_refs.iter().rev().group_by(|&hr| hr);
    let max_hand_refs = groups.into_iter().next();
    println!(
        "max_hand_refs: {:?}",
        max_hand_refs.map(|(_, group)| group.collect::<Vec<_>>())
    );
    let groups = hand_refs.iter().rev().group_by(|&hr| hr);
    let max_hand_refs = groups.into_iter().next();
    max_hand_refs.map(|(_, group)| group.map(|hr| hr.source()).collect())
}

fn parse_hand_refs<'a>(hand_strings: &[&'a str]) -> Option<Vec<HandRef<'a>>> {
    hand_strings
        .into_iter()
        .map(|&s| HandRef::from_string(s))
        .collect()
}
