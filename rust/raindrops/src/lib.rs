pub fn raindrops(n: u32) -> String {
    let sounds = RAINDROP_FACTOR_SOUNDS
        .iter()
        .fold(Vec::new(), |mut sounds, factor_sound| {
            add_sound(&mut sounds, n, factor_sound);
            sounds
        });
    if sounds.is_empty() {
        n.to_string()
    } else {
        sounds.join("")
    }
}

fn add_sound(sounds: &mut Vec<String>, number: u32, factor_sound: &(u32, &'static str)) {
    let (factor, sound) = factor_sound;
    if is_factor(number, *factor) {
        sounds.push(sound.to_string());
    }
}

const RAINDROP_FACTOR_SOUNDS: [(u32, &'static str); 3] = [(3, "Pling"), (5, "Plang"), (7, "Plong")];

fn is_factor(number: u32, factor: u32) -> bool {
    number % factor == 0
}
