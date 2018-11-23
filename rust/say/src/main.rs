extern crate say;

use std::io::{stdin,stdout,Write};

fn main() {
    let mut s;
    print!("Please enter a number: ");
    s = get_user_line();
    while s.parse::<u64>().is_err() {
        s = get_user_line();
    }
    let num: u64 = s.parse().unwrap();
    println!("{} is {}", num, say::encode(num));
}

fn get_user_line() -> String {
    let mut s = String::new();
    let _ = stdout().flush();
    stdin().read_line(&mut s).expect("Did not enter a correct string");
    if let Some('\n')=s.chars().next_back() {
        s.pop();
    }
    if let Some('\r')=s.chars().next_back() {
        s.pop();
    }
    s
}
