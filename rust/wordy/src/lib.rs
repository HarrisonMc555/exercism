#[macro_use]
extern crate nom;

pub struct WordProblem;

pub fn answer(input: &str) -> Option<i32> {
    let (remaining, result) = command(input).ok()?;
    if !remaining.is_empty() {
        return None;
    }
    Some(result)
}

#[derive(Debug)]
enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

// Combined
named!(expr(&str) -> i32, do_parse!(
    init: number >>
    res: fold_many0!(
        // Any operation, apply left to right
        tuple!(operator, number),
        init,
        |acc, (op, t)| apply_operation(acc, op, t)
    ) >>
    (res)
));
named!(command(&str) -> i32, do_parse!(
    prefix >>
    e: expr >>
    suffix >>
    (e)
));

// Prefixes and suffixes
named!(prefix(&str) -> (), value!((), tag!("What is")));
named!(suffix(&str) -> (), do_parse!(
    tag!("?") >>
    // eof!() >> // Not working for some reason...
    (())
));

// Numbers
named!(unsigned_digits(&str) -> &str,
    take_while!(|c: char| c.is_ascii_digit())
);
named!(signed_digits(&str) -> (Option<&str>, &str),
    pair!(
        opt!(alt!(tag!("+") | tag!("-"))),
        unsigned_digits
    )
);
named!(maybe_signed_digits(&str) -> &str,
    recognize!(signed_digits)
);
named!(int32(&str) -> i32,
    map_res!(maybe_signed_digits, str::parse)
);
named!(number(&str) -> i32, ws!(int32));

// Operators
named!(operator(&str) -> Operator,
    ws!(alt!(plus | minus | multiply | divide))
);
named!(plus(&str) -> Operator,
    value!(Operator::Plus, tag!("plus"))
);
named!(minus(&str) -> Operator,
    value!(Operator::Minus, tag!("minus"))
);
named!(multiply(&str) -> Operator,
    value!(Operator::Multiply, tag!("multiplied by"))
);
named!(divide(&str) -> Operator,
    value!(Operator::Divide, tag!("divided by"))
);

fn apply_operation(x: i32, op: Operator, y: i32) -> i32 {
    match op {
        Operator::Plus => x + y,
        Operator::Minus => x - y,
        Operator::Multiply => x * y,
        Operator::Divide => x / y,
    }
}
