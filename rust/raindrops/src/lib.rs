pub fn raindrops(n: u32) -> String {
    let sounds: Vec<&str> = RAINDROP_FACTOR_SOUNDS
        .iter()
        .filter(|&&(factor, _)| is_factor(n, factor))
        .map(|&(_, sound)| sound)
        .collect();
    if sounds.is_empty() {
        n.to_string()
    } else {
        sounds.join("")
    }
}

const RAINDROP_FACTOR_SOUNDS: [(u32, &str); 3] = [(3, "Pling"), (5, "Plang"), (7, "Plong")];

fn is_factor(number: u32, factor: u32) -> bool {
    number % factor == 0
}
