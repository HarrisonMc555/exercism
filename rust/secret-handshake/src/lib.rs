#[must_use]
pub fn actions(n: u8) -> Vec<&'static str> {
    let mut actions = Vec::new();
    for command in CommandFlag::from_bits(n)
        .expect("valid command flags")
        .to_commands()
    {
        command.perform(&mut actions);
    }
    // let flag_actions = [
    //     (0b00001, "wink"),
    //     (0b00010, "double blink"),
    //     (0b00100, "close your eyes"),
    //     (0b01000, "jump"),
    // ];
    // for (flag, action) in flag_actions {
    //     if n & flag != 0 {
    //         actions.push(action);
    //     }
    // }
    // if n & 0b10000 != 0 {
    //     actions.reverse();
    // }
    actions
}

use bitflags::bitflags;

bitflags! {
    // #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    struct CommandFlag: u8 {
        const WINK = 0b00001;
        const DOUBLE_BLINK = 0b00010;
        const CLOSE_YOUR_EYES = 0b00100;
        const JUMP = 0b01000;
        const REVERSE = 0b10000;
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum Command {
    Action(Action),
    Reverse,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
enum Action {
    Wink,
    DoubleBlink,
    CloseYourEyes,
    Jump,
}

impl Command {
    fn perform(self, actions: &mut Vec<&'static str>) {
        match self {
            Command::Action(action) => actions.push(action.string()),
            Command::Reverse => actions.reverse(),
        }
    }
}

impl Action {
    fn string(self) -> &'static str {
        match self {
            Action::Wink => "wink",
            Action::DoubleBlink => "double blink",
            Action::CloseYourEyes => "close your eyes",
            Action::Jump => "jump",
        }
    }
}

impl CommandFlag {
    const ALL: [CommandFlag; 5] = [
        CommandFlag::WINK,
        CommandFlag::DOUBLE_BLINK,
        CommandFlag::CLOSE_YOUR_EYES,
        CommandFlag::JUMP,
        CommandFlag::REVERSE,
        ];

    fn iter(&self) -> CommandFlagIter {
        CommandFlagIter {
            inner: CommandFlag::ALL.into_iter(),
            flag: *self,
        }
    }

    fn to_commands(self) -> impl Iterator<Item = Command> {
        self.iter().filter_map(|flag| {
            Some(match flag {
                CommandFlag::WINK => Command::Action(Action::Wink),
                CommandFlag::DOUBLE_BLINK => Command::Action(Action::DoubleBlink),
                CommandFlag::CLOSE_YOUR_EYES => Command::Action(Action::CloseYourEyes),
                CommandFlag::JUMP => Command::Action(Action::Jump),
                CommandFlag::REVERSE => Command::Reverse,
                _ => return None,
            })
        })
    }
}

struct CommandFlagIter {
    inner: std::array::IntoIter<CommandFlag, 5>,
    flag: CommandFlag,
}

impl Iterator for CommandFlagIter {
    type Item = CommandFlag;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let single_flag = self.inner.next()?;
            if self.flag.contains(single_flag) {
                return Some(single_flag);
            }
        }
    }
}