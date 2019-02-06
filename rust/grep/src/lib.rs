use failure::Error;

#[macro_use] extern crate failure;
#[macro_use] extern crate failure_derive;

/// While using raw slice of str to handle flags is convenient,
/// in the real-world projects it is customary to use a struct,
/// that contains flags-related logic. So in this exercise
/// we ask you to implement a custom struct.
///
/// If you are curious about real-world implementation, refer to the `clap-rs` crate:
/// https://github.com/kbknapp/clap-rs/blob/master/src/args/arg_matches.rs
#[derive(Debug)]
pub struct Flags;

impl Flags {
    pub fn new(flags: &[&str]) -> Self {
        Flags
    }
}

#[derive(Debug)]
enum GrepError {
    // #[fail(display = "file does not exist: {}", file)]
    FileDoesNotExist {
        file: String,
    },
}

pub fn grep(pattern: &str, flags: &Flags, files: &[&str]) -> Result<Vec<String>, Error> {
    files
        .iter()
        .map(|file| get_matches(pattern, flags, file))
        .collect::<Result<Vec<Vec<String>>, Error>>()
        .map(|vecs| vecs.into_iter().flatten().collect())
}

fn get_matches(pattern: &str, flags: &Flags, file: &str) -> Result<Vec<String>, Error> {
    // let e = GrepError::FileDoesNotExist(file.to_string());
    // Err(GrepError::FileDoesNotExist(file.to_string()))
    Err(format_err!("file does not exist: {}", file))
}

