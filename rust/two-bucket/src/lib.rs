use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

type State = (u8, u8);

/// A struct to hold your results in.
#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    /// The total number of "moves" it should take to reach the desired number of liters, including
    /// the first fill.
    pub moves: u8,
    /// Which bucket should end up with the desired number of liters? (Either "one" or "two")
    pub goal_bucket: Bucket,
    /// How many liters are left in the other bucket?
    pub other_bucket: u8,
}

/// Solve the bucket problem
pub fn solve(capacity_1: u8, capacity_2: u8, goal: u8, start_bucket: &Bucket) -> BucketStats {
    let starting_state = match &start_bucket {
        Bucket::One => (capacity_1, 0),
        Bucket::Two => (0, capacity_2),
    };
    let mut prev_states = HashMap::new();
    prev_states.insert((0, 0), 0);
    let _moves = next_moves(starting_state, &prev_states);
    unimplemented!(
        "Given one bucket of capacity {}, another of capacity {}, starting with {:?}, find pours to reach {}",
        capacity_1,
        capacity_2,
        start_bucket,
        goal,
    );
}

fn next_moves(state: State, prev_states: &HashMap<State, u8>) -> Vec<State> {
    let moves = possible_moves(state);
    moves.into_iter().filter(|st| !prev_states.contains_key(st)).collect()
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
