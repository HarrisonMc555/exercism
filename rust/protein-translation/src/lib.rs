use std::collections::HashMap;

pub struct CodonsInfo<'a> {
    codon_to_name: HashMap<&'a str, &'a str>,
}

impl<'a> CodonsInfo<'a> {
    pub fn name_for(&self, codon: &str) -> Option<&'a str> {
        // Only cloning the reference
        self.codon_to_name.get(codon).cloned()
    }

    pub fn of_rna(&self, rna: &str) -> Option<Vec<&'a str>> {
        let mut result = Vec::new();
        if rna.len() % 3 != 0 {
            return None;
        }
        for i in (0..rna.len() / 3).map(|i| i * 3) {
            let codon = &rna[i..i + 3];
            if let Some(name) = self.codon_to_name.get(&codon) {
                if name == &"stop codon" {
                    return Some(result);
                }
                result.push(*name);
            } else {
                return None;
            }
        }
        Some(result)
    }
}

pub fn parse<'a>(pairs: Vec<(&'a str, &'a str)>) -> CodonsInfo<'a> {
    CodonsInfo {
        codon_to_name: pairs.into_iter().collect(),
    }
}
