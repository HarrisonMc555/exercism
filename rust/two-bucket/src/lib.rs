use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

type Capacity = u8;
type State = (Capacity, Capacity);
type Moves = u8;

/// A struct to hold your results in.
#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    /// The total number of "moves" it should take to reach the desired number of liters, including
    /// the first fill.
    pub moves: Moves,
    /// Which bucket should end up with the desired number of liters? (Either "one" or "two")
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
    let mut states = HashMap::new();
    let zero_state = (0, 0);
    let first_state = match &start_bucket {
        Bucket::One => (capacity_1, 0),
        Bucket::Two => (0, capacity_2),
    };
    states.insert(zero_state, 0);
    states.insert(first_state, 1);
    let mut moves = vec![first_state];
    let mut num_moves = 1;
    loop {
        if let Some(state) = goal_bucket(&moves, goal) {
            return BucketStats {
                moves: num_moves,
                goal_bucket: get_goal_bucket(state, goal),
                other_bucket: get_other_capacity(state, goal),
            };
        }
        moves = all_next_moves(&moves, &states);
        num_moves += 1;
    }
}

fn all_next_moves(states: &Vec<State>, prev_states: &HashMap<State, Moves>) -> Vec<State> {
    let mut all_moves = Vec::new();
    for state in states.iter().cloned() {
        all_moves.extend(next_moves(state, prev_states));
    }
    dedupify(&mut all_moves);
    all_moves
}

fn next_moves(state: State, prev_states: &HashMap<State, Moves>) -> Vec<State> {
    let mut moves = possible_moves(state);
    moves.sort_unstable();
    moves.dedup();
    moves
        .into_iter()
        .filter(|st| !prev_states.contains_key(st))
        .collect()
}

fn possible_moves(state: State) -> Vec<State> {
    let mut moves = pouring_moves(state);
    moves.extend(emptying_moves(state));
    moves.extend(filling_moves(state));
    moves
}

fn pouring_moves(_state: State) -> Vec<State> {
    unimplemented!("pouring_moves not yet implemented");
}

fn emptying_moves(_state: State) -> Vec<State> {
    unimplemented!("emptying_moves not yet implemented");
}

fn filling_moves(_state: State) -> Vec<State> {
    unimplemented!("filling_moves not yet implemented");
}

fn goal_bucket(states: &Vec<State>, goal: Capacity) -> Option<State> {
    states.iter().find(|&&st| has_goal_bucket(st, goal)).map(|&st| st)
}

fn has_goal_bucket(state: State, goal: Capacity) -> bool {
    let (a, b) = state;
    a == goal || b == goal
}

fn get_goal_bucket(state: State, goal: Capacity) -> Bucket {
    let (a, b) = state;
    if a == goal {
        Bucket::One
    } else if b == goal {
        Bucket::Two
    } else {
        panic!("Tried to find goal bucket when neither had goal")
    }
}

fn get_other_capacity(state: State, goal: Capacity) -> Capacity {
    let (a, b) = state;
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
