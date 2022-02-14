#[derive(Debug, PartialEq, Clone)]
pub enum ResistorColor {
    Black,
    Brown,
    Red,
    Orange,
    Yellow,
    Green,
    Blue,
    Violet,
    Grey,
    White,
}
use ResistorColor::*;

pub fn color_to_value(color: ResistorColor) -> usize {
    match color {
        Black => 0,
        Brown => 1,
        Red => 2,
        Orange => 3,
        Yellow => 4,
        Green => 5,
        Blue => 6,
        Violet => 7,
        Grey => 8,
        White => 9,
    }
}

pub fn value_to_color_string(value: usize) -> String {
    match value {
        0 => "Black",
        1 => "Brown",
        2 => "Red",
        3 => "Orange",
        4 => "Yellow",
        5 => "Green",
        6 => "Blue",
        7 => "Violet",
        8 => "Grey",
        9 => "White",
        _ => "value out of range",
    }
    .to_string()
}

const ALL_COLORS: [ResistorColor; 10] = [
    Black, Brown, Red, Orange, Yellow, Green, Blue, Violet, Grey, White,
];
pub fn colors() -> Vec<ResistorColor> {
    ALL_COLORS.to_vec()
}
