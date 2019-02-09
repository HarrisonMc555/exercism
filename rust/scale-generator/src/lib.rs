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
}

impl Scale {
    pub fn new(tonic: &str, intervals: &str) -> Result<Scale, Error> {
        unimplemented!(
            "Construct a new scale with tonic {} and intervals {}",
            tonic,
            intervals
        )
    }

    pub fn chromatic(tonic: &str) -> Result<Scale, Error> {
        let tonic = Note::try_from(tonic).ok_or(())?;
        let mut notes = vec![tonic];
        let mut cur = tonic.half_step_up();
        while cur != tonic {
            notes.push(cur);
            cur = cur.half_step_up();
        }
        Ok(Scale { notes })
    }

    pub fn enumerate(&self) -> Vec<String> {
        self.notes.iter().map(|note| note.to_string(true)).collect()
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Note {
    note: u8,
}

impl Note {
    fn new(note: u8) -> Self {
        Note { note }
    }

    pub fn try_new(value: u8) -> Option<Self> {
        match Note::is_valid(value) {
            true => Some(Note::new(value)),
            false => None,
        }
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

    fn increased_by(&self, increase_value: u8) -> Self {
        let next_note = (self.note + increase_value) % Note::NUM_NOTES;
        Note::new(next_note)
    }

    fn decreased_by(&self, decrease_value: u8) -> Self {
        self.increased_by(Note::NUM_NOTES - decrease_value)
    }

    const NUM_NOTES: u8 = 12;

    fn is_valid(value: u8) -> bool {
        value < Note::NUM_NOTES
    }

    fn to_string(&self, ascending: bool) -> String {
        match (self.note, ascending) {
            (0, _) => "A",
            (1, true) => "A#",
            (1, false) => "Bb",
            (2, _) => "B",
            (3, _) => "C",
            (4, true) => "C#",
            (4, false) => "Db",
            (5, _) => "D",
            (6, true) => "D#",
            (6, false) => "Eb",
            (7, _) => "E",
            (8, _) => "F",
            (9, true) => "F#",
            (9, false) => "Gb",
            (10, _) => "G",
            (11, true) => "G#",
            (11, false) => "Ab",
            (_, _) => panic!("Invalid note value"),
        }.to_string()
    }

    fn try_from(string: &str) -> Option<Self> {
        let note = match string {
            "A" => Note::new(0),
            "A#" => Note::new(1),
            "Bb" => Note::new(1),
            "B" => Note::new(2),
            "C" => Note::new(3),
            "C#" => Note::new(4),
            "Db" => Note::new(4),
            "D" => Note::new(5),
            "D#" => Note::new(6),
            "Eb" => Note::new(6),
            "E" => Note::new(7),
            "F" => Note::new(8),
            "F#" => Note::new(9),
            "Gb" => Note::new(9),
            "G" => Note::new(10),
            "G#" => Note::new(11),
            "Ab" => Note::new(11),
            _ => return None,
        };
        Some(note)
    }
}
