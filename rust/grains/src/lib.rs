const BOARD_WIDTH: usize = 8;
const NUM_BOARD_SQUARES: u32 = (BOARD_WIDTH * BOARD_WIDTH) as u32;

pub fn square(s: u32) -> u64 {
    if s == 0 || s > NUM_BOARD_SQUARES {
        panic!("Square must be between 1 and 64");
    }
    0x1 << (s - 1)
}

pub fn total() -> u64 {
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
