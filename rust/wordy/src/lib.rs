#[macro_use]
extern crate nom;

#[derive(Debug)]
enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

named!(prefix(&str) -> &str, tag!("What is"));
// named!(suffix(&str) -> &str, terminated!(tag!("?"), eof!()));
named!(suffix(&str) -> &str, tag!("?"));

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
// named!(number(&str) -> i32, map_res!(
//     take_while!(|c: char| c.is_ascii_digit()),
//     str::parse
// ));
named!(number(&str) -> i32, alt!(int32));
named!(factor(&str) -> i32, ws!(number));
named!(term(&str) -> i32, do_parse!(
    init: factor >>
    res: fold_many0!(
        tuple!(alt!(multiply | divide), factor),
        init,
        |acc, (op, t)| apply_operation(acc, op, t)
    ) >>
    (res)
));
named!(expr(&str) -> i32, do_parse!(
    init: term >>
    res: fold_many0!(
        tuple!(alt!(plus | minus), term),
        init,
        |acc, (op, t)| apply_operation(acc, op, t)
    ) >>
    (res)
));

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

named!(command(&str) -> i32, do_parse!(
    prefix >>
    e: expr >>
    suffix >>
    (e)
));

fn apply_operation(x: i32, op: Operator, y: i32) -> i32 {
    match op {
        Operator::Plus => x + y,
        Operator::Minus => x - y,
        Operator::Multiply => x * y,
        Operator::Divide => x / y,
    }
}

pub struct WordProblem;

pub fn answer(input: &str) -> Option<i32> {
    let (_, result) = command(input).ok()?;
    Some(result)
}

pub fn get_question(command: &str) -> Option<&str> {
    const PREFIX: &str = "What is ";
    const SUFFIX: &str = "?";
    if !command.starts_with(PREFIX) {
        return None;
    }
    let command = &command[PREFIX.len()..];
    if !command.ends_with(SUFFIX) {
        return None;
    }
    Some(&command[..command.len() - SUFFIX.len()])
}
