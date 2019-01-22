const BOARD_WIDTH: usize = 8;
const NUM_BOARD_SQUARES: u32 = (BOARD_WIDTH * BOARD_WIDTH) as u32;

pub fn square(s: u32) -> u64 {
    if s == 0 || s > NUM_BOARD_SQUARES {
        panic!("Square must be between 1 and 64");
    }
    // To raise 2 to a power, you simply left shift by that amount. Since we
    // want to raise 2 to one less than the board number (1 indexed vs. 0
    // indexed problems) we subtract one first.
    1 << (s - 1)
}

pub fn total() -> u64 {
    // The sum from 1 to n of 2^(n-1) is 2^n - 1.
    // For 64 board squares, this is 2^64 - 1.
    // 2^64 in binary is 1 followed by 64 zeros, and if you subtract 1 it is
    // simply 64 ones. In a 64 bit number, this means every bit is a 1. We can
    // do this easily by simply doing a bitwise negation on a value where every
    // bit is a 0, i.e. 0.
    !0
}

fn _naive_square(s: u32) -> u64 {
    if s == 0 || s > NUM_BOARD_SQUARES {
        panic!("Square must be between 1 and 64");
    }
    u64::pow(2, s - 1)
}

fn _naive_total() -> u64 {
    (1..=NUM_BOARD_SQUARES).map(|x| square(x as u32)).sum()
}
