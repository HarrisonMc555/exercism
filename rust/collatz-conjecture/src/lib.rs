pub fn collatz(n: u64) -> Option<u64> {
    if n == 0 {
        return None;
    }
    let mut x = n;
    let mut count = 0;
    loop {
        match x {
            1 => return Some(count),
            old_x => x = next_collatz(old_x),
        }
        count += 1;
    }
}

fn next_collatz(n: u64) -> u64 {
    if n % 2 == 0 {
        n / 2
    } else {
        3 * n + 1
    }
}
