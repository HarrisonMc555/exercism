#[derive(Debug)]
pub struct ChessPosition {
    rank: i32,
    file: i32,
}

#[derive(Debug)]
pub struct Queen(ChessPosition);

impl ChessPosition {
    #![allow(clippy::new_ret_no_self)]    
    pub fn new(rank: i32, file: i32) -> Option<Self> {
        if ChessPosition::rank_in_range(rank)
            && ChessPosition::file_in_range(file)
        {
            Some(ChessPosition { rank, file })
        } else {
            None
        }
    }

    pub fn same_rank_as(&self, position: &ChessPosition) -> bool {
        self.rank == position.rank
    }

    pub fn same_file_as(&self, position: &ChessPosition) -> bool {
        self.file == position.file
    }

    pub fn same_forward_diagonal_as(&self, position: &ChessPosition) -> bool {
        let rank_diff = self.rank - position.rank;
        let file_diff = self.file - position.file;
        rank_diff == file_diff
    }

    pub fn same_backward_diagonal_as(&self, position: &ChessPosition) -> bool {
        let rank_diff = self.rank - position.rank;
        let file_diff = self.file - position.file;
        rank_diff == -file_diff
    }

    const MIN_RANK: i32 = 0;
    const MAX_RANK: i32 = 7;

    fn rank_in_range(rank: i32) -> bool {
        in_range(&rank, &ChessPosition::MIN_RANK, &ChessPosition::MAX_RANK)
    }

    const MIN_FILE: i32 = 0;
    const MAX_FILE: i32 = 7;

    fn file_in_range(file: i32) -> bool {
        in_range(&file, &ChessPosition::MIN_FILE, &ChessPosition::MAX_FILE)
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Queen(position)
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        let self_pos = &self.0;
        let other_pos = &other.0;
        self_pos.same_rank_as(&other_pos)
            || self_pos.same_file_as(&other_pos)
            || self_pos.same_forward_diagonal_as(&other_pos)
            || self_pos.same_backward_diagonal_as(&other_pos)
    }
}

fn in_range<T: PartialOrd>(value: &T, min: &T, max: &T) -> bool {
    min <= value && value <= max
}
