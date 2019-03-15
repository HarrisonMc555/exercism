pub struct WordProblem;

pub fn answer(command: &str) -> Option<i32> {
    let question = get_question(command)?;
    question.parse().ok()
}

pub fn get_question(command: &str) -> Option<&str> {
    const PREFIX: &str = "What is ";
    const SUFFIX: &str = "?";
    // if !command.starts_with()
    if !command.starts_with(PREFIX) {
        return None;
    }
    let command = &command[PREFIX.len()..];
    if !command.ends_with(SUFFIX) {
        return None;
    }
    Some(&command[..command.len() - SUFFIX.len()])
}
