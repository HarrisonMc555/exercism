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
    println!("winning_hands.A");
    let mut hand_refs = parse_hand_refs(hands)?;
    println!("winning_hands.A.1");
    hand_refs.sort_unstable_by_key(|hand_ref| hand_ref.score());
    println!("winning_hands.B");
    hand_refs.reverse(); // High-to-low
    println!("winning_hands.C");
    // let winning_hand_refs = all_highest_scoring(hand_refs)?;
    // let winning_string_refs = winning_hand_refs
    //     .iter()
    //     .map(|hand_ref| hand_ref.source())
    //     .collect();
    // Some(winning_string_refs)
    // let mut hand_refs = parse_hand_refs(hands)?;
    // println!("winning_hands.B");
    // println!("winning_hands.C");
    // println!(
    //     "hand_refs: {}",
    //     hand_refs
    //         .iter()
    //         .map(|hr| hr.source())
    //         .collect::<Vec<_>>()
    //         .join(", ")
    // );
    // println!("winning_hands.D");
    // for (score, group) in &hand_refs.into_iter().group_by(|hr| hr.score()) {
    //     println!(
    //         "score: {:?} {}",
    //         score.partial_score(),
    //         score.remaining_hand()
    //     );
    //     for hand_ref in group {
    //         println!("\thand_ref: {}", hand_ref.source());
    //     }
    // }
    // let groups: itertools::GroupBy<crate::hand::HandScore, _, _> =
    //     hand_refs.iter().group_by(|hr| hr.score());
    // println!(
    //     "{:?} {:?} {:?}",
    //     hand_refs[0].score(),
    //     hand_refs[0].score().partial_cmp(&hand_refs[1].score()),
    //     hand_refs[1].score()
    // );
    // for (score, group) in &groups {
    //     println!(
    //         "score: {:?} {}",
    //         score.partial_score(),
    //         score.remaining_hand()
    //     );
    //     for hand_ref in group {
    //         println!("\thand_ref: {}", hand_ref.source());
    //     }
    // }
    // for window in hand_refs.windows(2) {
    //     let h1_score = window[0].score();
    //     let h2_score = window[1].score();
    //     println!(
    //         "{:?} {:?} {:?}",
    //         h1_score.partial_score(),
    //         h1_score.partial_cmp(&h2_score),
    //         h2_score.partial_score()
    //     );
    // }
    let groups = hand_refs.iter().group_by(|&hr| hr.score());
    println!("winning_hands.D");
    // println!("winning_hands.E");
    let (_, highest_scoring_hand_refs) = groups.into_iter().next()?;
    println!("winning_hands.E");
    // println!("winning_hands.F");
    let strings = highest_scoring_hand_refs.map(|hr| hr.source()).collect();
    println!("winning_hands.F");
    // println!("winning_hands.G");
    Some(strings)
}

fn parse_hand_refs<'a>(hand_strings: &[&'a str]) -> Option<Vec<HandRef<'a>>> {
    hand_strings
        .into_iter()
        .map(|&s| HandRef::from_string(s))
        .collect()
}

// fn all_highest_scoring<'a>(
//     hand_refs: Vec<HandRef<'a>>,
// ) -> Option<Vec<HandRef<'a>>> {
//     let mut hand_refs = hand_refs;
//     hand_refs.sort_unstable_by_key(|hand_ref| hand_ref.score());
//     hand_refs.reverse();
//     let high_score = hand_refs.iter().next()?.score();
//     // let all_highest_scoring = hand_refs
//     //     .into_iter()
//     //     .take_while(|&&hand_ref| hand_ref.score() == high_score)
//     //     .collect();
//     // Some(all_highest_scoring)
//     Some(
//         hand_refs
//             .into_iter()
//             .take_while(|hand_ref| hand_ref.score() == high_score)
//             .collect(),
//     )
// }
