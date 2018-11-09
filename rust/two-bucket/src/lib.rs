use std::cmp;
use std::collections::HashSet;

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

type Capacity = u8;
type State = (Capacity, Capacity);
type Limit = (Capacity, Capacity);
type Moves = u8;

/// A struct to hold your results in.
#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    /// The total number of "moves" it should take to reach the desired number
    /// of liters, including the first fill.
    pub moves: Moves,
    /// Which bucket should end up with the desired number of liters? (Either
    /// "one" or "two")
    pub goal_bucket: Bucket,
    /// How many liters are left in the other bucket?
    pub other_bucket: Capacity,
}

/// Solve the bucket problem
pub fn solve(
    capacity_1: Capacity,
    capacity_2: Capacity,
    goal: u8,
    start_bucket: &Bucket,
) -> BucketStats {
    let zero_state = (0, 0);
    let first_state = match &start_bucket {
        Bucket::One => (capacity_1, 0),
        Bucket::Two => (0, capacity_2),
    };
    let mut prev_states = HashSet::new();
    prev_states.insert(zero_state);
    prev_states.insert(first_state);
    let mut cur_states = vec![first_state];
    let mut num_moves = 1;
    let limit = (capacity_1, capacity_2);
    loop {
        println!("Move {}:", num_moves);
        for state in &cur_states {
            let (a, b) = state;
            println!("\t({}/{}, {}/{})", a, capacity_1, b, capacity_2);
        }
        if let Some(state) = goal_bucket(&cur_states, goal) {
            return BucketStats {
                moves: num_moves,
                goal_bucket: get_goal_bucket(&state, goal),
                other_bucket: get_other_capacity(&state, goal),
            };
        }
        prev_states.extend(&cur_states);
        cur_states = all_next_moves(&cur_states, &limit, &prev_states);
        cur_states = remove_opposite_start(cur_states, &limit, &start_bucket);
        num_moves += 1;
    }
}

fn all_next_moves(
    states: &Vec<State>,
    limit: &Limit,
    prev_states: &HashSet<State>,
) -> Vec<State> {
    let mut all_moves = Vec::new();
    for state in states.iter().cloned() {
        all_moves.extend(next_moves(&state, limit, prev_states));
    }
    dedupify(&mut all_moves);
    all_moves
}

fn next_moves(
    state: &State,
    limit: &Limit,
    prev_states: &HashSet<State>,
) -> Vec<State> {
    let mut moves = possible_moves(state, limit);
    moves.sort_unstable();
    moves.dedup();
    moves
        .into_iter()
        .filter(|st| !prev_states.contains(st))
        .collect()
}

fn possible_moves(state: &State, limit: &Limit) -> Vec<State> {
    let mut moves = pouring_moves(state, limit);
    moves.extend(emptying_moves(state, limit));
    moves.extend(filling_moves(state, limit));
    moves
}

fn pouring_moves(state: &State, limit: &Limit) -> Vec<State> {
    let &(a, b) = state;
    let &(a_max, b_max) = limit;
    let mut moves = Vec::new();
    if a < a_max {
        let a_new = cmp::min(a_max, a + b);
        let a_diff = a_new - a;
        let b_new = b - a_diff;
        moves.push((a_new, b_new));
    }
    if b < b_max {
        let b_new = cmp::min(b_max, b + a);
        let b_diff = b_new - b;
        let a_new = a - b_diff;
        moves.push((a_new, b_new));
    }
    moves
}

fn emptying_moves(state: &State, _limit: &Limit) -> Vec<State> {
    let &(a, b) = state;
    let mut moves = Vec::new();
    if a > 0 {
        moves.push((0, b));
    }
    if b > 0 {
        moves.push((a, 0));
    }
    moves
}

fn filling_moves(state: &State, limit: &Limit) -> Vec<State> {
    let &(a, b) = state;
    let &(a_max, b_max) = limit;
    let mut moves = Vec::new();
    if a < a_max {
        moves.push((a_max, b));
    }
    if b < b_max {
        moves.push((a, b_max));
    }
    moves
}

fn goal_bucket(states: &Vec<State>, goal: Capacity) -> Option<State> {
    states
        .iter()
        .find(|&st| has_goal_bucket(st, goal))
        .map(|&st| st)
}

fn has_goal_bucket(state: &State, goal: Capacity) -> bool {
    let &(a, b) = state;
    a == goal || b == goal
}

fn get_goal_bucket(state: &State, goal: Capacity) -> Bucket {
    let &(a, b) = state;
    if a == goal {
        Bucket::One
    } else if b == goal {
        Bucket::Two
    } else {
        panic!("Tried to find goal bucket when neither had goal")
    }
}

fn get_other_capacity(state: &State, goal: Capacity) -> Capacity {
    let &(a, b) = state;
    if a == goal {
        b
    } else if b == goal {
        a
    } else {
        panic!("Tried to find other bucket when neither had goal")
    }
}

fn dedupify<T>(vec: &mut Vec<T>)
where
    T: Ord,
{
    vec.sort_unstable();
    vec.dedup()
}

fn remove_opposite_start(
    states: Vec<State>,
    limit: &Limit,
    start_bucket: &Bucket,
) -> Vec<State> {
    states
        .iter()
        .filter(|&st| !is_opposite_start(st, &limit, &start_bucket))
        .map(|&st| st)
        .collect()
}

fn is_opposite_start(
    state: &State,
    limit: &Limit,
    start_bucket: &Bucket,
) -> bool {
    let &(a, b) = state;
    let &(a_max, b_max) = limit;
    match start_bucket {
        Bucket::One => a == 0 && b == b_max,
        Bucket::Two => a == a_max && b == 0,
    }
}
