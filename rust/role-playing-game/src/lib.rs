// This stub file contains items which aren't used yet; feel free to remove this module attribute
// to enable stricter warnings.
#![allow(unused)]

pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        if self.health != 0 {
            return None;
        }
        let mana = if self.level < 10 { None } else { Some(100) };
        Some(Player {
            health: 100,
            mana,
            level: self.level,
        })
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            None => {
                self.health = self.health.saturating_sub(mana_cost);
                0
            }
            Some(mana) if mana < mana_cost => 0,
            Some(mana) => {
                self.mana = Some(mana - mana_cost);
                2 * mana_cost
            }
        }
    }
}
