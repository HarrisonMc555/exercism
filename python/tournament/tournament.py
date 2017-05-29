# constants
WIN = 'win'
LOSS = 'loss'
DRAW = 'draw'
POINTS = {WIN: 3,
          LOSS: 0,
          DRAW: 1}
OPPOSITE_RESULT = {WIN: LOSS,
                   LOSS: WIN,
                   DRAW: DRAW}
HEADER = 'Team                           | MP |  W |  D |  L |  P'


# functions
def tally(data):
    """Create analysis string of points, wins, losses, etc."""
    teams = parse_input(data)
    output = [HEADER]
    for name, result in sorted(teams.items(), key=team_sort):
        output.append(results_string(name, result))
    return '\n'.join(output)


def parse_input(data):
    """Parse team data out of string list of games."""
    if not data:
        return {}
    teams = {}
    for line in data.split('\n'):
        team1_name, team2_name, team1_result = line.split(';')
        team2_result = OPPOSITE_RESULT[team1_result]
        add_result(teams, team1_name, team1_result)
        add_result(teams, team2_name, team2_result)
    return teams


def add_result(teams, name, game_result):
    """Add result of game to both teams' record."""
    cur_results = teams.get(name, empty_results())
    cur_results[game_result] += 1
    teams[name] = cur_results


def results_string(name, results):
    """Create a formatted result string for a single team."""
    points = tally_points(results)
    num_matches = total_matches(results)
    return '{0: <31}|{1: >3} |{2: >3} |{3: >3} |{4: >3} |{5: >3}'.format(
        name, num_matches, results[WIN], results[DRAW], results[LOSS], points)


def tally_points(results):
    """Return total points earned."""
    return sum([POINTS[result]*num for result, num in results.items()])


def total_matches(results):
    """Return total number of matches played."""
    return sum(results.values())


def empty_results():
    """Create empty results dict."""
    # Need to make this a function (or inline where it appears), because if we
    # use a global variable then each person who tries to initialize a results
    # object will get the same object.
    return {WIN: 0, LOSS: 0, DRAW: 0}


def team_sort(tup):
    """Order team from most to fewest points then alphabetically."""
    name, result = tup
    # Negate points to make higher points come first while keeping name
    # alphabetical
    return (-tally_points(result), name)
