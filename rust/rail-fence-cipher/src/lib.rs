pub struct RailFence {
    num_rails: usize,
}

impl RailFence {
    pub fn new(rails: u32) -> RailFence {
        RailFence {
            num_rails: rails as usize,
        }
    }

    pub fn encode(&self, text: &str) -> String {
        let mut encoded = String::new();
        let letters = text.chars().collect::<Vec<_>>();
        let num_letters = letters.len();
        let cycle_length = self.cycle_length();
        for index in (0..num_letters).step_by(cycle_length) {
            encoded.extend(letters.get(index));
        }
        for rail in 1..self.num_rails - 1 {
            for index1 in (rail..num_letters).step_by(cycle_length) {
                encoded.extend(letters.get(index1));
                let index2 = index1 + cycle_length - rail * 2;
                encoded.extend(letters.get(index2));
            }
        }
        for index in ((cycle_length / 2)..num_letters).step_by(cycle_length) {
            encoded.extend(letters.get(index));
        }
        encoded
    }

    pub fn decode(&self, cipher: &str) -> String {
        if self.num_rails < 2 {
            return cipher.to_string();
        }
        let letters = cipher.chars().collect::<Vec<_>>();
        let num_letters = letters.len();
        let cycle_length = self.cycle_length();
        let num_cycles = div_ceil(num_letters, cycle_length);

        let rail_starting_indices = self.rail_starting_indices(num_letters);
        let bottom_rail_starting_index = rail_starting_indices[self.num_rails - 1];

        let mut result = Vec::new();
        'outer: for cycle_index in 0..num_cycles {
            // Top rail
            let top_rail_letter_index = cycle_index;
            result.push(letters[top_rail_letter_index]);
            if result.len() >= num_letters {
                break;
            }
            // Middle rails going down
            for rail_index in 1..self.num_rails - 1 {
                let rail_letter_index = rail_starting_indices[rail_index] + 2 * cycle_index;
                result.push(letters[rail_letter_index]);
                if result.len() >= num_letters {
                    break 'outer;
                }
            }
            // Bottom rail
            let bottom_rail_letter_index = bottom_rail_starting_index + cycle_index;
            result.push(letters[bottom_rail_letter_index]);
            if result.len() >= num_letters {
                break;
            }
            // Middle rails going up
            for rail_index in (1..self.num_rails - 1).rev() {
                let rail_letter_index = rail_starting_indices[rail_index] + 2 * cycle_index + 1;
                result.push(letters[rail_letter_index]);
                if result.len() >= num_letters {
                    break 'outer;
                }
            }
        }
        result.into_iter().collect()
    }

    fn cycle_length(&self) -> usize {
        if self.num_rails <= 1 {
            1
        } else {
            (self.num_rails - 1) * 2
        }
    }

    fn rail_starting_indices(&self, num_letters: usize) -> Vec<usize> {
        let cycle_length = self.cycle_length();
        let num_complete_cycles = num_letters / cycle_length;
        let num_complete_cycle_letters = num_complete_cycles * cycle_length;
        let mut num_remaining_letters = num_letters - num_complete_cycle_letters;

        let mut rail_lengths = vec![num_complete_cycles * 2; self.num_rails];
        rail_lengths[0] = num_complete_cycles;
        rail_lengths[self.num_rails - 1] = num_complete_cycles;

        if num_remaining_letters > 0 {
            num_remaining_letters -= 1;
            rail_lengths[0] += 1;
        }
        for rail_index in 1..self.num_rails - 1 {
            if num_remaining_letters > 0 {
                num_remaining_letters -= 1;
                rail_lengths[rail_index] += 1;
            } else {
                break;
            }
        }
        for rail_index in (1..self.num_rails).rev() {
            if num_remaining_letters > 0 {
                num_remaining_letters -= 1;
                rail_lengths[rail_index] += 1;
            } else {
                break;
            }
        }

        rail_lengths.into_iter().scan(0, |state, length| {
            let index = *state;
            *state = index + length;
            Some(index)
        }).collect()
    }
}


fn div_ceil(dividend: usize, divisor: usize) -> usize {
    (dividend + divisor - 1) / divisor
}
