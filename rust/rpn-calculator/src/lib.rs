#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    use std::ops::{Add, Div, Mul, Sub};
    use CalculatorInput::*;
    let mut stack = Vec::new();
    for input in inputs {
        let new_value = match input {
            Add => binary_op(&mut stack, Add::add),
            Subtract => binary_op(&mut stack, Sub::sub),
            Multiply => binary_op(&mut stack, Mul::mul),
            Divide => binary_op(&mut stack, Div::div),
            Value(value) => Some(*value),
        }?;
        stack.push(new_value)
    }
    match stack[..] {
        [value] => Some(value),
        _ => None,
    }
}

fn binary_op(stack: &mut Vec<i32>, op: fn(i32, i32) -> i32) -> Option<i32> {
    let y = stack.pop()?;
    let x = stack.pop()?;
    Some(op(x, y))
}
