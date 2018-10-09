use std::cmp::Ordering;

const NUM_FRAMES: usize = 10;
const NUM_PINS: u16 = 10;

#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Frame {
    first_roll: u16,
    second_roll: Option<u16>,
}

#[derive(Debug, PartialEq)]
pub struct BowlingGame {
    frames: [Option<Frame>; NUM_FRAMES],
    index: usize,
}

impl BowlingGame {
    pub fn new() -> Self {
        BowlingGame {
            frames: [None; NUM_FRAMES],
            index: 0,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if self.is_complete() {
            return Err(Error::GameComplete);
        }
        let cur_frame = self.frames[self.index];
        if let Some(cur_frame) = cur_frame {
            let new_frame = cur_frame.add_roll(pins)?;
            self.frames[self.index] = Some(new_frame);
            self.index += 1;
        } else {
            let new_frame = Frame::new(pins)?;
            self.frames[self.index] = Some(new_frame);
            if !new_frame.is_unfinished() {
                self.index += 1;
            }
        }
        Ok(())
    }

    pub fn score(&self) -> Option<u16> {
        if self.is_complete() {
            Some(self.score_so_far())
        } else {
            None
        }
    }

    fn is_complete(&self) -> bool {
        self.index == NUM_FRAMES
    }

    fn score_so_far(&self) -> u16 {
        self.frames
            .iter()
            .map(|op_frame| op_frame.map_or(0, |f| f.score().unwrap_or(0)))
            .sum()
    }
}

impl Frame {
    fn new(first_roll: u16) -> Result<Self, Error> {
        if first_roll > NUM_PINS {
            Err(Error::NotEnoughPinsLeft)
        } else {
            Ok(Frame { first_roll: first_roll,
                       second_roll: None})
        }
    }

    fn add_roll(&self, new_roll: u16) -> Result<Self, Error> {
        match self.second_roll {
            Some(_) => Err(Error::NotEnoughPinsLeft),
            None => Ok(Self { second_roll: Some(new_roll), ..*self })
        }
    }

    fn score(&self, next_rolls: (u16, u16)) -> u16 {
        let (first_roll, second_roll) = next_rolls;
        if self.is_strike() {
            NUM_PINS + first_roll + second_roll
        } else if self.is_spare() {
            NUM_PINS + first_roll
        } else {
            self.first_roll + self.second_roll.unwrap_or(0)
        }
    }

    fn is_strike(&self) -> bool {
        self.first_roll == NUM_PINS
    }

    fn is_spare(&self) -> bool {
        self.first_roll + self.second_roll.unwrap_or(0) == NUM_PINS
    }

    fn is_finished(&self) -> bool {
        self.first_roll == NUM_PINS || !self.second_roll.is_none()
    }

    fn is_unfinished(&self) -> bool {
        !self.is_finished()
    }

    fn two_rolls(&self, next_frame: Frame) -> (u16, u16) {
        let second_roll = match self.second_roll {
            Some(second_roll) => second_roll,
            None => next_frame.first_roll
        };
        (self.first_roll, second_roll)
    }
}
