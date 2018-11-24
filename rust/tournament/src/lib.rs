use std::collections::HashMap;

const HEADER: &'static str = "Team                           | MP |  W |  D |  L |  P";

type Match = (String, String, MatchResult);
type History = (u64, u64, u64);

#[derive(Debug, PartialEq)]
enum MatchResult {
    Win,
    Loss,
    Draw,
}

pub fn tally(match_results: &str) -> String {
    let mut results = HashMap::new();
    for line in match_results.lines() {
        let result = get_result(line);
        add_result(&mut results, result);
    }
    let mut results: Vec<_> = results.iter().collect();
    results.sort_unstable_by_key(|(_, history)| num_points(history));
    results.reverse();
    let mut lines = vec![String::from(HEADER)];
    for (name, &history) in results.iter() {
        lines.push(create_line(name, history));
    }
    lines.join("\n")
}

fn get_result(match_result: &str) -> (String, String, MatchResult) {
    let words: Vec<_> = match_result.split(";").collect();
    let team1 = String::from(words[0]);
    let team2 = String::from(words[1]);
    let result = words[2];
    let result = match result {
        "win" => MatchResult::Win,
        "loss" => MatchResult::Loss,
        "draw" => MatchResult::Draw,
        _ => panic!("invalid match result string"),
    };
    (team1, team2, result)
}

fn add_result(results: &mut HashMap<String, History>, match_: Match) {
    let (team1, team2, result) = match_;
    let other_result = other_team_result(&result);
    {
        let team1_history = results.entry(team1).or_insert_with(|| (0, 0, 0));
        update_history(team1_history, result);
    }
    let team2_history = results.entry(team2).or_insert_with(|| (0, 0, 0));
    update_history(team2_history, other_result);
}

fn other_team_result(result: &MatchResult) -> MatchResult {
    match result {
        MatchResult::Win => MatchResult::Loss,
        MatchResult::Loss => MatchResult::Win,
        MatchResult::Draw => MatchResult::Draw,
    }
}

fn create_line(team_name: &str, history: History) -> String {
    let (wins, losses, draws) = history;
    let points = num_points(&history);
    let matches = num_matches(&history);
    format!(
        "{:30} | {:2} | {:2} | {:2} | {:2} | {:2}",
        team_name, matches, wins, draws, losses, points
    )
}

fn update_history(history: &mut History, result: MatchResult) {
    match result {
        MatchResult::Win => history.0 += 1,
        MatchResult::Loss => history.1 += 1,
        MatchResult::Draw => history.2 += 1,
    }
}

fn num_points((wins, _, draws): &History) -> u64 {
    wins * 3 + draws
}

fn num_matches((wins, losses, draws): &History) -> u64 {
    wins + losses + draws
}
