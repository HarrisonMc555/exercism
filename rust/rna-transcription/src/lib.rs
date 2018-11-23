#[derive(Debug, PartialEq)]
enum DnaNuc {
    G,
    C,
    T,
    A,
}

#[derive(Debug, PartialEq)]
enum RnaNuc {
    C,
    G,
    A,
    U,
}

#[derive(Debug, PartialEq)]
pub struct DNA {
    nucleotides: Vec<DnaNuc>,
}

#[derive(Debug, PartialEq)]
pub struct RNA {
    nucleotides: Vec<RnaNuc>,
}

impl DNA {
    pub fn new(dna: &str) -> Result<DNA, usize> {
        dna.chars()
            .enumerate()
            .map(|(i, c)| DnaNuc::new(c).ok_or(i))
            .collect::<Result<_, _>>()
            .map(|ns| DNA { nucleotides: ns })
    }

    pub fn to_rna(&self) -> RNA {
        RNA {
            nucleotides: self.nucleotides.iter().map(DnaNuc::to_rna).collect(),
        }
    }
}

impl RNA {
    pub fn new(rna: &str) -> Result<RNA, usize> {
        rna.chars()
            .enumerate()
            .map(|(i, c)| RnaNuc::new(c).ok_or(i))
            .collect::<Result<_, _>>()
            .map(|ns| RNA { nucleotides: ns })
    }
}

impl DnaNuc {
    fn new(nuc: char) -> Option<Self> {
        match nuc {
            'G' => Some(DnaNuc::G),
            'C' => Some(DnaNuc::C),
            'T' => Some(DnaNuc::T),
            'A' => Some(DnaNuc::A),
            _ => None,
        }
    }

    fn to_rna(&self) -> RnaNuc {
        match self {
            DnaNuc::G => RnaNuc::C,
            DnaNuc::C => RnaNuc::G,
            DnaNuc::T => RnaNuc::A,
            DnaNuc::A => RnaNuc::U,
        }
    }
}

impl RnaNuc {
    fn new(nuc: char) -> Option<Self> {
        match nuc {
            'C' => Some(RnaNuc::C),
            'G' => Some(RnaNuc::G),
            'A' => Some(RnaNuc::A),
            'U' => Some(RnaNuc::U),
            _ => None,
        }
    }
}
