use failure::Error;
use itertools::Itertools;
use regex::Regex;
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
    only_file_names: bool,
    case_insensitive: bool,
    invert: bool,
    entire_line: bool,
}

impl Flags {
    pub fn new(flags: &[&str]) -> Self {
        Flags {
            line_numbers: flags.contains(&"-n"),
            only_file_names: flags.contains(&"-l"),
            case_insensitive: flags.contains(&"-i"),
            invert: flags.contains(&"-v"),
            entire_line: flags.contains(&"-x"),
        }
    }

    pub fn line_numbers(&self) -> bool {
        self.line_numbers
    }

    pub fn only_file_names(&self) -> bool {
        self.only_file_names
    }

    pub fn case_insensitive(&self) -> bool {
        self.case_insensitive
    }

    pub fn invert(&self) -> bool {
        self.invert
    }

    pub fn entire_line(&self) -> bool {
        self.entire_line
    }
}

pub fn grep(
    pattern: &str,
    flags: &Flags,
    files: &[&str],
) -> Result<Vec<String>, Error> {
    let pattern_re = compile_regex_from_flags(&pattern, &flags)?;
    files
        .iter()
        .map(|file| get_matches(&pattern_re, flags, file))
        .fold_results(Vec::new(), |mut all_matches, matches| {
            all_matches.extend(matches);
            all_matches
        })
}

fn get_matches(
    pattern: &Regex,
    flags: &Flags,
    filename: &str,
) -> Result<Vec<String>, Error> {
    let file = File::open(filename)?;
    let lines = BufReader::new(file).lines();
    let mut matches = Vec::new();
    for (line_num, line) in lines.enumerate().map(|(num, line)| (num + 1, line))
    {
        let line = line?;
        if !should_match(&pattern, &line, &flags) {
            continue;
        }
        if flags.only_file_names() {
            return Ok(vec![filename.to_string()]);
        }
        let mut result = String::new();
        if flags.line_numbers() {
            result.push_str(&format!("{}:", line_num));
        }
        result.push_str(&line);
        matches.push(result);
    }
    Ok(matches)
}

fn should_match(pattern: &Regex, line: &str, flags: &Flags) -> bool {
    if flags.invert() {
        !pattern.is_match(&line)
    } else {
        pattern.is_match(&line)
    }
}

fn compile_regex_from_flags(
    pattern: &str,
    flags: &Flags,
) -> Result<Regex, Error> {
    let case_flag = if flags.case_insensitive() { "(?i)" } else { "" };
    let flagged_pattern = if flags.entire_line() {
        format!("^{}{}$", case_flag, pattern)
    } else {
        format!("{}{}", case_flag, pattern)
    };
    let regex = Regex::new(&flagged_pattern)?;
    Ok(regex)
}
