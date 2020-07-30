use std::collections::HashMap;

pub type Value = i32;
pub type ForthResult = Result<(), Error>;
type CustomCommands = HashMap<String, Vec<Command>>;

const ADD: &str = "+";
const SUBTRACT: &str = "-";
const MULTIPLY: &str = "*";
const DIVIDE: &str = "/";
const DUP: &str = "dup";
const DROP: &str = "drop";
const SWAP: &str = "swap";
const OVER: &str = "over";
const CUSTOM_BEGIN: &str = ":";
const CUSTOM_END: &str = ";";

#[derive(Default)]
pub struct Forth {
    stack: Vec<Value>,
    custom_commands: CustomCommands,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    DivisionByZero,
    StackUnderflow,
    UnknownWord,
    InvalidWord,
}

#[derive(Debug, PartialEq, Clone)]
enum Command {
    Literal(Value),
    Add,
    Subtract,
    Multiply,
    Divide,
    Dup,
    Drop,
    Swap,
    Over,
    Custom(Vec<Command>),
    Definition(String, Vec<Command>),
}

impl Forth {
    pub fn new() -> Forth {
        Forth {
            stack: Vec::new(),
            custom_commands: HashMap::new(),
        }
    }

    pub fn stack(&self) -> Vec<Value> {
        self.stack.clone()
    }

    pub fn eval(&mut self, input: &str) -> ForthResult {
        let mut iter = input.split_ascii_whitespace();
        while let Some(result) = self.parse_command(&mut iter) {
            let command = result?;
            self.eval_command(command)?;
        }
        Ok(())
    }

    fn parse_command(
        &self,
        iter: &mut std::str::SplitAsciiWhitespace,
    ) -> Option<Result<Command, Error>> {
        let word = iter.next()?.to_ascii_lowercase();
        let result = if word == CUSTOM_BEGIN {
            self.parse_custom_definition(iter)
        } else {
            self.parse_single_command(word)
        };
        Some(result)
    }

    fn parse_single_command(&self, word: String) -> Result<Command, Error> {
        let command = if let Some(commands) = self.custom_commands.get(&word) {
            Command::Custom(commands.clone())
        } else {
            match word.as_ref() {
                ADD => Command::Add,
                SUBTRACT => Command::Subtract,
                MULTIPLY => Command::Multiply,
                DIVIDE => Command::Divide,
                DUP => Command::Dup,
                DROP => Command::Drop,
                SWAP => Command::Swap,
                OVER => Command::Over,
                w => {
                    return w
                        .parse()
                        .map(Command::Literal)
                        .map_err(|_| Error::UnknownWord)
                }
            }
        };
        Ok(command)
    }

    fn parse_custom_definition(
        &self,
        iter: &mut std::str::SplitAsciiWhitespace,
    ) -> Result<Command, Error> {
        let custom_command = iter.next().ok_or(Error::InvalidWord)?.to_ascii_lowercase();
        // It is illegal to redefined a literal value
        if custom_command.parse::<Value>().is_ok() {
            return Err(Error::InvalidWord);
        }
        let mut commands = Vec::new();
        for word in iter {
            let word = word.to_ascii_lowercase();
            if word == CUSTOM_END {
                return Ok(Command::Definition(custom_command, commands));
            } else {
                commands.push(self.parse_single_command(word)?);
            }
        }
        // If we never found the CUSTOM_END, then this is an illegal definition
        Err(Error::InvalidWord)
    }

    fn eval_command(&mut self, command: Command) -> ForthResult {
        match command {
            Command::Literal(value) => self.literal(value),
            Command::Add => self.add(),
            Command::Subtract => self.subtract(),
            Command::Multiply => self.multiply(),
            Command::Divide => self.divide(),
            Command::Dup => self.dup(),
            Command::Drop => self.drop(),
            Command::Swap => self.swap(),
            Command::Over => self.over(),
            Command::Custom(commands) => self.custom(commands),
            Command::Definition(name, commands) => self.definition(name, commands),
        }
    }

    fn literal(&mut self, value: Value) -> ForthResult {
        self.stack.push(value);
        Ok(())
    }

    fn add(&mut self) -> ForthResult {
        let (x, y) = self.get_operands()?;
        self.stack.push(x + y);
        Ok(())
    }

    fn subtract(&mut self) -> ForthResult {
        let (x, y) = self.get_operands()?;
        self.stack.push(x - y);
        Ok(())
    }

    fn multiply(&mut self) -> ForthResult {
        let (x, y) = self.get_operands()?;
        self.stack.push(x * y);
        Ok(())
    }

    fn divide(&mut self) -> ForthResult {
        let (x, y) = self.get_operands()?;
        if y == 0 {
            return Err(Error::DivisionByZero);
        }
        self.stack.push(x / y);
        Ok(())
    }

    fn dup(&mut self) -> ForthResult {
        let value = self.stack().last().copied().ok_or(Error::StackUnderflow)?;
        self.stack.push(value);
        Ok(())
    }

    fn drop(&mut self) -> ForthResult {
        self.stack.pop().map(|_| ()).ok_or(Error::StackUnderflow)
    }

    fn swap(&mut self) -> ForthResult {
        if self.stack.len() < 2 {
            return Err(Error::StackUnderflow);
        }
        let len = self.stack.len();
        self.stack.swap(len - 1, len - 2);
        Ok(())
    }

    fn over(&mut self) -> ForthResult {
        let over = self
            .stack
            .iter()
            .rev()
            .nth(1) // second to last
            .copied()
            .ok_or(Error::StackUnderflow)?;
        self.stack.push(over);
        Ok(())
    }

    fn custom(&mut self, commands: Vec<Command>) -> ForthResult {
        commands.into_iter().map(|c| self.eval_command(c)).collect()
    }

    fn definition(&mut self, name: String, commands: Vec<Command>) -> ForthResult {
        self.custom_commands.insert(name, commands);
        Ok(())
    }

    fn get_operands(&mut self) -> Result<(Value, Value), Error> {
        let last_2 = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let last_1 = self.stack.pop().ok_or(Error::StackUnderflow)?;
        Ok((last_1, last_2))
    }
}
