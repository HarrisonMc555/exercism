use failure::Error;
use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader};

/// While using raw slice of str to handle flags is convenient,
/// in the real-world projects it is customary to use a struct,
/// that contains flags-related logic. So in this exercise
/// we ask you to implement a custom struct.
///
/// If you are curious about real-world implementation, refer to the `clap-rs` crate:
/// https://github.com/kbknapp/clap-rs/blob/master/src/args/arg_matches.rs
#[derive(Debug)]
pub struct Flags {
    line_numbers: bool,
}

impl Flags {
    pub fn new(flags: &[&str]) -> Self {
        Flags {
            line_numbers: flags.contains(&"-n"),
        }
    }

    pub fn include_line_numbers(&self) -> bool {
        self.line_numbers
    }
}

pub fn grep(
    pattern: &str,
    flags: &Flags,
    files: &[&str],
) -> Result<Vec<String>, Error> {
    files
        .iter()
        .map(|file| get_matches(pattern, flags, file))
        .fold_results(Vec::new(), |mut all_matches, matches| {
            all_matches.extend(matches);
            all_matches
        })
}

fn get_matches(
    pattern: &str,
    flags: &Flags,
    file: &str,
) -> Result<Vec<String>, Error> {
    let file = File::open(file)?;
    let lines = BufReader::new(file).lines();
    let mut matches = Vec::new();
    for (line_num, line) in lines.enumerate().map(|(num, line)| (num + 1, line))
    {
        let line = line?;
        if line.contains(pattern) {
            let result = if flags.include_line_numbers() {
                format!("{}:{}", line_num, line)
            } else {
                line
            };
            matches.push(result);
        }
    }
    Ok(matches)
}
