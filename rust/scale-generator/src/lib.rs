// You should change this.
//
// Depending on your implementation, there are a variety of potential errors
// which might occur. They aren't checked by the test suite in order to
// allow the greatest freedom of implementation, but real libraries should
// provide useful, descriptive errors so that downstream code can react
// appropriately.
//
// One common idiom is to define an Error enum which wraps all potential
// errors. Another common idiom is to use a helper type such as failure::Error
// which does more or less the same thing but automatically.
pub type Error = ();

pub struct Scale {
    notes: Vec<Note>,
    // modality: Modality,
}

#[derive(Debug, Copy, Clone)]
enum Modality {
    Major,
    Minor,
}

impl Scale {
    pub fn new(tonic: &str, intervals: &str) -> Result<Scale, Error> {
        // let modality = Scale::tonic_modality(tonic);
        let tonic = Note::try_from_string(tonic).ok_or(())?;
        let intervals = intervals
            .chars()
            .map(|c| Interval::try_from_char(c).ok_or(()))
            .collect::<Result<Vec<_>, _>>()?;
        // let mut notes = tonic.build_from_intervals(&intervals, &modality);
        let mut notes = tonic.build_from_intervals(&intervals);
        if notes.first().unwrap() == notes.last().unwrap() {
            notes.pop();
        }
        // Ok(Scale { notes, modality })
        Ok(Scale { notes })
    }

    pub fn chromatic(tonic: &str) -> Result<Scale, Error> {
        // let modality = Scale::tonic_modality(tonic);
        let tonic = Note::try_from_string(tonic).ok_or(())?;
        let mut notes = vec![tonic];
        let mut cur = tonic.half_step_up();
        while cur != tonic {
            notes.push(cur);
            cur = cur.half_step_up();
        }
        // Ok(Scale { notes, modality })
        Ok(Scale { notes })
    }

    pub fn enumerate(&self) -> Vec<String> {
        self.notes.iter().map(|note| note.to_string()).collect()
    }

    fn tonic_modality(tonic: &str) -> Modality {
        let is_major = tonic
            .chars()
            .next()
            .map(|c| c.is_ascii_uppercase())
            .unwrap_or(true);
        Modality::from_is_major(is_major)
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Note {
    note: u8,
    modifier: NoteModifier,
}

impl Note {
    fn new(note: u8, modifier: NoteModifier) -> Self {
        Note { note, modifier }
    }

    pub fn try_new(value: u8, modifier: NoteModifier) -> Option<Self> {
        match Note::is_valid_value(value) {
            true => Some(Note::new(value, modifier)),
            false => None,
        }
    }

    pub fn build_from_intervals(&self, intervals: &[Interval]) -> Vec<Note> {
        let mut notes = vec![*self];
        let mut cur = *self;
        for &interval in intervals {
            cur = cur.offset_by(interval);
            notes.push(cur);
        }
        notes
    }

    pub fn half_step_up(&self) -> Self {
        self.increased_by(1)
    }

    pub fn half_step_down(&self) -> Self {
        self.decreased_by(1)
    }

    pub fn whole_step_up(&self) -> Self {
        self.increased_by(2)
    }

    pub fn whole_step_down(&self) -> Self {
        self.decreased_by(2)
    }

    pub fn offset_by(&self, interval: Interval) -> Self {
        let interval = interval.0;
        if interval < 0 {
            self.decreased_by(interval.abs() as u8)
        } else {
            self.increased_by(interval as u8)
        }
    }

    fn increased_by(&self, increase_value: u8) -> Self {
        let next_note = (self.note + increase_value) % Note::NUM_NOTES;
        Note::new(next_note, self.modifier)
    }

    fn decreased_by(&self, decrease_value: u8) -> Self {
        self.increased_by(Note::NUM_NOTES - decrease_value)
    }

    const NUM_NOTES: u8 = 12;

    fn is_valid_value(value: u8) -> bool {
        value < Note::NUM_NOTES
    }

    fn to_string(&self) -> String {
        match (self.note, self.modifier) {
            (0, _) => "A",
            (1, NoteModifier::Sharp) => "A#",
            (1, NoteModifier::Flat) => "Bb",
            (2, _) => "B",
            (3, _) => "C",
            (4, NoteModifier::Sharp) => "C#",
            (4, NoteModifier::Flat) => "Db",
            (5, _) => "D",
            (6, NoteModifier::Sharp) => "D#",
            (6, NoteModifier::Flat) => "Eb",
            (7, _) => "E",
            (8, _) => "F",
            (9, NoteModifier::Sharp) => "F#",
            (9, NoteModifier::Flat) => "Gb",
            (10, _) => "G",
            (11, NoteModifier::Sharp) => "G#",
            (11, NoteModifier::Flat) => "Ab",
            (_, _) => panic!("Invalid note value {:?}", self),
        }
        .to_string()
    }

    fn try_from_string(s: &str) -> Option<Self> {
        // let is_major = s.chars().next()?.is_ascii_uppercase();
        // let modality = Modality::from_is_major(is_major);
        let note = match s {
            "C" => Note::new(3, NoteModifier::Sharp),
            "a" => Note::new(0, NoteModifier::Sharp),
            "G" => Note::new(10, NoteModifier::Sharp),
            "D" => Note::new(5, NoteModifier::Sharp),
            "A" => Note::new(0, NoteModifier::Sharp),
            "E" => Note::new(7, NoteModifier::Sharp),
            "B" => Note::new(2, NoteModifier::Sharp),
            "F#" => Note::new(9, NoteModifier::Sharp),
            "e" => Note::new(7, NoteModifier::Sharp),
            "b" => Note::new(2, NoteModifier::Sharp),
            "f#" => Note::new(9, NoteModifier::Sharp),
            "c#" => Note::new(4, NoteModifier::Sharp),
            "g#" => Note::new(11, NoteModifier::Sharp),
            "d#" => Note::new(6, NoteModifier::Sharp),
            "F" => Note::new(8, NoteModifier::Flat),
            "Bb" => Note::new(1, NoteModifier::Flat),
            "Eb" => Note::new(6, NoteModifier::Flat),
            "Ab" => Note::new(11, NoteModifier::Flat),
            "Db" => Note::new(4, NoteModifier::Flat),
            "Gb" => Note::new(9, NoteModifier::Flat),
            "d" => Note::new(5, NoteModifier::Flat),
            "g" => Note::new(10, NoteModifier::Flat),
            "c" => Note::new(3, NoteModifier::Flat),
            "f" => Note::new(8, NoteModifier::Flat),
            "bb" => Note::new(1, NoteModifier::Flat),
            "eb" => Note::new(6, NoteModifier::Flat),
            _ => return None,
        };
        Some(note)
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Interval(pub i8);

impl Interval {
    fn try_from_char(c: char) -> Option<Interval> {
        let interval = match c {
            'm' => Interval(1),
            'M' => Interval(2),
            'A' => Interval(3),
            _ => return None,
        };
        Some(interval)
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
enum NoteModifier {
    Natural,
    Sharp,
    Flat,
}

fn uppercase_first_letter(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

impl Modality {
    fn from_is_major(is_major: bool) -> Self {
        match is_major {
            true => Modality::Major,
            false => Modality::Minor,
        }
    }
}
