use clap::{App, Arg, ArgMatches};
use std::io::{self, Error};

mod lib;
use lib::rotate;

fn main() -> Result<(), Error> {
    let args = parse_args();
    let rotate_amount_string = args.value_of("ROTATE").unwrap();
    let rotate_amount = rotate_amount_string.parse::<i8>().expect("Invalid number");
    let mut buffer = String::new();
    while let Ok(bytes) = io::stdin().read_line(&mut buffer) {
        if bytes == 0 {
            break;
        }
        println!("{}", rotate(&buffer, rotate_amount));
        buffer.clear();
    }
    Ok(())
}

fn parse_args() -> ArgMatches<'static> {
    App::new("Rotational Cipher")
        .version("1.0")
        .author("Harrison McCullough <mccullough.harrison@gmail.com>")
        .about("Performs an alhpabetical rotational cipher")
        .arg(
            Arg::with_name("ROTATE")
                .help("The amount to rotate each letter")
                .required(true)
                .index(1),
        )
        .get_matches()
}
