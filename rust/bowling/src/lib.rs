use std::cmp::Ordering;

const NUM_FRAMES: usize = 10;
const MAX_ROLLS_PER_FRAME: usize = 2;
const MAX_ROLLS_LAST_FRAME: usize = MAX_ROLLS_PER_FRAME + 1;
const MAX_ROLLS: usize = (NUM_FRAMES * MAX_ROLLS_PER_FRAME) + 1;
const NUM_PINS: u16 = 10;

#[derive(Debug, PartialEq)]
pub enum Error {
    NotEnoughPinsLeft,
    GameComplete,
}

#[derive(Default, Debug, PartialEq)]
pub struct BowlingGame {
    rolls: Vec<u16>,
    frame_indices: Vec<usize>,
}

impl BowlingGame {
    pub fn new() -> Self {
        let rolls = Vec::with_capacity(MAX_ROLLS);
        let mut frame_indices = Vec::with_capacity(NUM_FRAMES);
        frame_indices.push(0);
        BowlingGame {
            rolls,
            frame_indices,
        }
    }

    pub fn roll(&mut self, pins: u16) -> Result<(), Error> {
        if self.is_complete() {
            return Err(Error::GameComplete);
        }
        if self.is_invalid_roll(pins) {
            return Err(Error::NotEnoughPinsLeft);
        }
        self.rolls.push(pins);
        if self.cur_frame_is_finished() && !self.is_complete() {
            self.frame_indices.push(self.rolls.len());
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
        self.frame_indices.len() >= NUM_FRAMES && self.cur_frame_is_finished()
    }

    fn is_valid_roll(&self, pins: u16) -> bool {
        if self.is_last_frame() {
            return self.is_valid_last_frame(pins);
        }
        pins + self.cur_frame_roll_total() <= NUM_PINS
    }

    fn is_invalid_roll(&self, pins: u16) -> bool {
        !self.is_valid_roll(pins)
    }

    fn is_valid_last_frame(&self, pins: u16) -> bool {
        if pins > NUM_PINS {
            return false;
        }
        if self.cur_frame_pins().len() >= MAX_ROLLS_LAST_FRAME {
            return false;
        }
        let last_frame_index = self.last_frame_index();
        let first_roll = match self.rolls.get(last_frame_index) {
            Some(&roll) => roll,
            None => return true,
        };
        let first_is_strike = first_roll == NUM_PINS;
        let second_roll = match self.rolls.get(last_frame_index + 1) {
            Some(&roll) => roll,
            None => return true,
        };
        let first_is_spare = first_roll + second_roll == NUM_PINS;
        let second_is_strike = second_roll == NUM_PINS;

        if first_is_strike && second_is_strike {
            true
        } else if first_is_strike {
            second_roll + pins <= NUM_PINS
        } else {
            first_is_spare
        }
    }

    fn cur_frame_is_finished(&self) -> bool {
        if self.is_last_frame() {
            return self.is_last_frame_finished();
        }
        self.cur_frame_pins().len() == MAX_ROLLS_PER_FRAME
            || self.cur_frame_roll_total() == NUM_PINS
    }

    fn is_last_frame(&self) -> bool {
        self.frame_indices.len() == NUM_FRAMES
    }

    fn is_last_frame_finished(&self) -> bool {
        let num_rolls = self.cur_frame_pins().len();
        match num_rolls.cmp(&MAX_ROLLS_PER_FRAME) {
            Ordering::Less => false,
            Ordering::Equal => self.cur_frame_roll_total() < NUM_PINS,
            Ordering::Greater => true,
        }
    }

    fn cur_frame_roll_total(&self) -> u16 {
        self.cur_frame_pins().iter().sum()
    }

    fn cur_frame_pins(&self) -> &[u16] {
        &self.rolls[self.last_frame_index()..]
    }

    fn last_frame_index(&self) -> usize {
        // We start with one and we never remove, so this should never panic
        self.frame_indices
            .last()
            .cloned()
            .expect("Empty frame indices vector")
    }

    fn score_so_far(&self) -> u16 {
        self.frame_indices
            .iter()
            .map(|&i| self.score_frame(i).unwrap_or(0))
            .sum()
    }

    fn score_frame(&self, frame_index: usize) -> Option<u16> {
        let first_roll = self.rolls[frame_index];
        if first_roll == NUM_PINS {
            return self.score_strike(frame_index);
        }
        let second_roll = self.rolls.get(frame_index + 1)?;
        let frame_total = first_roll + second_roll;
        if frame_total == NUM_PINS {
            self.score_spare(frame_index)
        } else {
            Some(frame_total)
        }
    }

    fn score_strike(&self, roll_index: usize) -> Option<u16> {
        let next_roll = self.rolls.get(roll_index + 1)?;
        let next_next_roll = self.rolls.get(roll_index + 2)?;
        Some(NUM_PINS + next_roll + next_next_roll)
    }

    fn score_spare(&self, roll_index: usize) -> Option<u16> {
        let next_roll = self.rolls.get(roll_index + 2)?;
        Some(NUM_PINS + next_roll)
    }
}
