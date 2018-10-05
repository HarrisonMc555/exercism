// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[derive(PartialEq, Debug)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

pub struct Robot {
    position: (i32, i32),
    direction: Direction,
}

impl Robot {
    pub fn new(x: i32, y: i32, d: Direction) -> Self {
        Robot {
            position: (x, y),
            direction: d,
        }
    }

    pub fn turn_right(self) -> Self {
        Self {
            direction: next_direction_clockwise(&self.direction),
            ..self
        }
    }

    pub fn turn_left(self) -> Self {
        Self {
            direction: next_direction_counterclockwise(&self.direction),
            ..self
        }
    }

    pub fn advance(self) -> Self {
        let (x, y) = self.position;
        let (dx, dy) = direction_position_delta(&self.direction);
        Self {
            position: (x + dx, y + dy),
            ..self
        }
    }

    pub fn instructions(self, instructions: &str) -> Self {
        instructions
            .chars()
            .fold(self, |robot, instruction| robot.instruction(instruction))
    }

    pub fn position(&self) -> (i32, i32) {
        self.position
    }

    pub fn direction(&self) -> &Direction {
        &self.direction
    }

    fn instruction(self, instruction: char) -> Self {
        match instruction {
            'L' => self.turn_left(),
            'R' => self.turn_right(),
            'A' => self.advance(),
            _ => panic!("Unknown instruction '{}'", instruction),
        }
    }
}

fn next_direction_clockwise(direction: &Direction) -> Direction {
    match *direction {
        Direction::North => Direction::East,
        Direction::East => Direction::South,
        Direction::South => Direction::West,
        Direction::West => Direction::North,
    }
}

fn next_direction_counterclockwise(direction: &Direction) -> Direction {
    match *direction {
        Direction::North => Direction::West,
        Direction::East => Direction::North,
        Direction::South => Direction::East,
        Direction::West => Direction::South,
    }
}

fn direction_position_delta(direction: &Direction) -> (i32, i32) {
    match *direction {
        Direction::North => (0, 1),
        Direction::East => (1, 0),
        Direction::South => (0, -1),
        Direction::West => (-1, 0),
    }
}
