pub fn raindrops(n: u32) -> String {
    let mut sounds: Vec<String> = Vec::new();
    for factor_sound in RAINDROP_FACTOR_SOUNDS.iter() {
        add_sound(&mut sounds, n, factor_sound);
    }
    // I wanted to do something like this, but couldn't figure it out.
    // RAINDROP_FACTOR_SOUNDS.iter().fold(&mut sounds, |&mut v, factor_sound| {
    //     add_sound(&mut v, n, factor_sound);
    //     v
    // });
    if sounds.is_empty() {
        n.to_string()
    } else {
        sounds.join("")
    }
}

fn add_sound(sounds: &mut Vec<String>, number: u32,
             factor_sound: &(u32, &'static str)) {
    let (factor, sound) = factor_sound;
    if is_factor(number, *factor) {
        sounds.push(sound.to_string());
    }
}

const RAINDROP_FACTOR_SOUNDS: [(u32, &'static str); 3] = [
    (3, "Pling"),
    (5, "Plang"),
    (7, "Plong"),
];

fn is_factor(number: u32, factor: u32) -> bool {
    number % factor == 0
}
