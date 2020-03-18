type Score = u32;

#[derive(Debug)]
pub struct HighScores {
    scores: Vec<Score>,
    sorted_scores: Vec<Score>,
}

impl HighScores {
    pub fn new(scores: &[Score]) -> Self {
        let mut sorted_scores = scores.to_vec();
        sorted_scores.sort();
        HighScores {
            scores: scores.to_vec(),
            sorted_scores,
        }
    }

    pub fn scores(&self) -> &[Score] {
        &self.scores
    }

    pub fn latest(&self) -> Option<Score> {
        self.scores.last().copied()
    }

    pub fn personal_best(&self) -> Option<Score> {
        self.sorted_scores.last().copied()
    }

    pub fn personal_top_three(&self) -> Vec<Score> {
        self.sorted_scores.iter().rev().take(3).copied().collect()
    }
}
