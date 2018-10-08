use std::cmp::Ordering;

const NUM_FRAMES: usize = 10;
const NUM_PINS: u16 = 10;

#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Frame {
    Unfinished(u16),
    Open(u16, u16),
    Spare(u16),
    Strike,
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
            let new_frame = cur_frame.add_roll(pins).unwrap();
            self.frames[self.index] = Some(new_frame);
            self.index += 1;
        } else {
            let new_frame = Frame::new(pins, None)?;
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
        self.frames()
            .windows(3)
            .
        self.frames
            .iter()
            .map(|op_frame| op_frame.map_or(0, |f| f.score().unwrap_or(0)))
            .sum()
    }
}

impl Frame {
    fn new(first_roll: u16, second_roll: Option<u16>) -> Result<Self, Error> {
        if let Some(second_roll) = second_roll {
            let total = first_roll + second_roll;
            assert!(total <= NUM_PINS);
            if total == NUM_PINS {
                Ok(Frame::Spare(first_roll))
            } else {
                Ok(Frame::Open(first_roll, second_roll))
            }
        } else {
            match first_roll.cmp(&NUM_PINS) {
                Ordering::Greater => Err(Error::NotEnoughPinsLeft),
                Ordering::Equal => Ok(Frame::Strike),
                Ordering::Less => Ok(Frame::Unfinished(first_roll))
            }
        }
    }

    fn add_roll(&self, new_roll: u16) -> Result<Self, Error> {
        if let Frame::Unfinished(first_roll) = self {
            Self::new(*first_roll, Some(new_roll))
        } else {
            Err(Error::NotEnoughPinsLeft)
        }
    }

    fn score(&self, next_rolls: (u16, u16)) -> Option<u16> {
        match *self {
            Frame::Open(first, second) => Some(first + second),
            Frame::Spare(_) => Some(NUM_PINS),
            Frame::Strike => Some(NUM_PINS),
            Frame::Unfinished(_) => None,
        }
    }

    fn is_unfinished(&self) -> bool {
        match self {
            Frame::Unfinished(_) => true,
            _ => false,
        }
    }

    fn two_rolls(&self, next_frame: Option<Frame>) -> Option<(u16, u16)> {
        match *self {
            Frame::Open(first, second) => Some((first, second)),
            Frame::Spare(first) => Some((first, NUM_PINS - first)),
            Frame::Strike => next_frame.map(|f| (NUM_PINS, f.one_roll())),
            Frame::Unfinished(_) => None,
        }
    }

    fn one_roll(&self) -> u16 {
        match *self {
            Frame::Open(first, _) => first,
            Frame::Spare(first) => first,
            Frame::Strike => NUM_PINS,
            Frame::Unfinished(first) => first,
        }
    }
}
