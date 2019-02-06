const BASE: u32 = 10;

/// Type implementing arbitrary-precision decimal arithmetic
#[derive(Eq, PartialEq)]
pub struct Decimal {
    leading: Vec<u8>,
    trailing: Vec<u8>,
}

impl Decimal {
    pub fn try_from(input: &str) -> Option<Decimal> {
        let mut leading = Vec::new();
        let mut chars = input.chars();
        loop {
            match chars.next() {
                Some(c) => match c {
                    '0'..='9' => leading.push(c.to_digit(BASE)? as u8),
                    '.' => break,
                    _ => return None,
                },
                None => return Some(Decimal { leading, trailing: Vec::new() }),
            }
        }

        let mut trailing = Vec::new();
        for c in chars {
            if !c.is_ascii_digit() {
                return None;
            } else {
                trailing.push(c.to_digit(BASE)? as u8);
            }
        }
        Some(Decimal { leading, trailing })
    }
}
