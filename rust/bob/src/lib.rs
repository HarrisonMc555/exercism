pub fn reply(message: &str) -> &str {
    match message_type(message) {
        MessageType::Question => "Sure.",
        MessageType::Yelling => "Whoa, chill out!",
        MessageType::YellingQuestion => "Calm down, I know what I'm doing!",
        MessageType::Nothing => "Fine. Be that way!",
        MessageType::Other => "Whatever.",
    }
}

fn message_type(message: &str) -> MessageType {
    let yelling = is_yelling(message);
    let question = is_question(message);
    let nothing = is_nothing(message);
    // if yelling && question {
    //     MessageType::YellingQuestion
    // } else if yelling {
    //     MessageType::Yelling
    // } else if question {
    //     MessageType::Question
    // } else if nothing {
    //     MessageType::Nothing
    // } else {
    //     MessageType::Other
    // }
    match (yelling, question, nothing) {
        (true, true, _) => MessageType::YellingQuestion,
        (true, _, _) => MessageType::Yelling,
        (_, true, _) => MessageType::Question,
        (_, _, true) => MessageType::Nothing,
        _ => MessageType::Other,
    }
}

fn is_yelling(message: &str) -> bool {
    let has_uppercase = message.chars().any(char::is_uppercase);
    let has_lowercase = message.chars().any(char::is_lowercase);
    has_uppercase && !has_lowercase
}

fn is_question(message: &str) -> bool {
    message.trim().ends_with("?")
}

fn is_nothing(message: &str) -> bool {
    message.trim().is_empty()
}

enum MessageType {
    Question,
    Yelling,
    YellingQuestion,
    Nothing,
    Other,
}
