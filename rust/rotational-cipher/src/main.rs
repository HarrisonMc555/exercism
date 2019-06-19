use clap::{App, Arg, ArgMatches};
use std::io::{self, Error, Read};

mod lib;
use lib::rotate;

fn main() -> Result<(), Error> {
    let args = parse_args();
    let rotate_amount_string = args.value_of("ROTATE").unwrap();
    let rotate_amount = rotate_amount_string.parse::<i8>().expect("Invalid number");
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    // let converted = rotate(&buffer, rotate_amount);
    // println!("Text received: <<{}>>", buffer);
    // println!("Converted:     <<{}>>", converted);
    print!("{}", rotate(&buffer, rotate_amount));
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
