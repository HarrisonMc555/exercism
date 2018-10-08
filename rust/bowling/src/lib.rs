#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

const NUM_FRAMES: usize = 10;

#[derive(Debug, PartialEq, Clone, Copy)]
struct Frame {
    rolls: (u16, Option<u16>),
}

pub struct BowlingGame {
    frames: [Frame; NUM_FRAMES],
    index: usize,
}

impl BowlingGame {
    pub fn new() -> Self {
        BowlingGame {
            frames: [Frame {}; NUM_FRAMES],
            index: 0,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        unimplemented!("Record that {} pins have been scored", pins);
    }

    pub fn score(&self) -> Option<u16> {
        if self.is_complete() {
            Some(self.score_game())
        } else {
            None
        }
    }

    fn is_complete(&self) -> bool {
        self.index == NUM_FRAMES
    }

    fn score_game(&self) -> u16 {
        0
    }
}

impl Frame {
    pub fn new(first_roll: u16) -> Self {
        Frame { rolls: (first_roll, None) }
    }
}
