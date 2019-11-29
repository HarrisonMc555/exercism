pub type Value = i32;
pub type ForthResult = Result<(), Error>;

const PLUS: &str = "+";
const MINUS: &str = "-";
const MULTIPLY: &str = "*";
const DIVIDE: &str = "/";
const DUP: &str = "DUP";
const DROP: &str = "DROP";
const SWAP: &str = "SWAP";
const OVER: &str = "OVER";

pub struct Forth {
    stack: Vec<Value>,
    // commands: HashMap<String, String>,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

trait Command {
    fn eval(&self, forth: &mut Forth) -> ForthResult;
}

impl Forth {
    pub fn new() -> Forth {
        Forth { stack: Vec::new() }
    }

    pub fn stack(&self) -> Vec<Value> {
        self.stack.clone()
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        input
            .split_whitespace()
            .fold(Ok(()), |_, command| self.eval_command(command))
    }

    fn eval_command(&mut self, input: &str) -> ForthResult {
        if let Ok(number) = input.parse() {
            self.stack.push(number);
        } else if input.eq_ignore_ascii_case(PLUS) {
            let (x, y) = self.get_operands()?;
            self.stack.push(x + y);
        } else if input.eq_ignore_ascii_case(MINUS) {
            let (x, y) = self.get_operands()?;
            self.stack.push(x - y);
        } else if input.eq_ignore_ascii_case(MULTIPLY) {
            let (x, y) = self.get_operands()?;
            self.stack.push(x * y);
        } else if input.eq_ignore_ascii_case(DIVIDE) {
            let (x, y) = self.get_operands()?;
            if y == 0 {
                return Err(Error::DivisionByZero);
            }
            self.stack.push(x / y);
        } else if input.eq_ignore_ascii_case(DUP) {
            let value = self.stack().last().copied().ok_or(Error::StackUnderflow)?;
            self.stack.push(value);
        } else if input.eq_ignore_ascii_case(DROP) {
            self.stack.pop().ok_or(Error::StackUnderflow)?;
        } else if input.eq_ignore_ascii_case(SWAP) {
            if self.stack.len() < 2 {
                return Err(Error::StackUnderflow);
            }
            let len = self.stack.len();
            self.stack.swap(len - 1, len - 2);
        } else if input.eq_ignore_ascii_case(OVER) {
            let over = self
                .stack
                .iter()
                .rev()
                .nth(1) // second to last
                .copied()
                .ok_or(Error::StackUnderflow)?;
            self.stack.push(over);
        } else {
            return Err(Error::InvalidWord);
        }
        Ok(())
    }

    fn get_operands(&mut self) -> Result<(Value, Value), Error> {
        let last_2 = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let last_1 = self.stack.pop().ok_or(Error::StackUnderflow)?;
        Ok((last_1, last_2))
    }
}
