# Score categories
# Change the values as you see fit
YACHT = 'YACHT'
ONES = 'ONES'
TWOS = 'TWOS'
THREES = 'THREES'
FOURS = 'FOURS'
FIVES = 'FIVES'
SIXES = 'SIXES'
FULL_HOUSE = 'FULL'
FOUR_OF_A_KIND = 'FOUR'
LITTLE_STRAIGHT = 'LITTLE'
BIG_STRAIGHT = 'BIG'
CHOICE = 'CHOICE'


def score(dice, category):
    is_f = TESTING_FUNCTIONS[category]
    scoring_f = SCORING_FUNCTIONS[category]
    if not is_f(dice):
        return 0
    return scoring_f(dice)


# Scoring functions

def score_number(dice, number):
    d = to_dict(dice)
    n = d.get(number, 0)
    return n * number


def score_full_house(dice):
    return sum(dice)


def score_four_of_a_kind(dice):
    d = to_dict(dice)
    number = [die for die, n in d.items() if n >= 4][0]
    return number * 4  # always counts as four


def score_little_straight(dice):
    return 30


def score_big_straight(dice):
    return 30


def score_choice(dice):
    return sum(dice)


def score_yacht(dice):
    return 50


# Testing functions

def is_number(dice, number):
    return True


def is_full_house(dice):
    d = to_dict(dice)
    if len(d) != 2:
        return False
    return sorted(d.values()) == [2, 3]


def is_four_of_a_kind(dice):
    d = to_dict(dice)
    most = max(d.values())
    return most >= 4


def is_little_straight(dice):
    return sorted(dice) == [1, 2, 3, 4, 5]


def is_big_straight(dice):
    return sorted(dice) == [2, 3, 4, 5, 6]


def is_choice(dice):
    return True


def is_yacht(dice):
    d = to_dict(dice)
    most = max(d.values())
    return most >= 5


# Category dictionaries

TESTING_FUNCTIONS = {
    YACHT: is_yacht,
    ONES: lambda dice: is_number(dice, 1),
    TWOS: lambda dice: is_number(dice, 2),
    THREES: lambda dice: is_number(dice, 3),
    FOURS: lambda dice: is_number(dice, 4),
    FIVES: lambda dice: is_number(dice, 5),
    SIXES: lambda dice: is_number(dice, 6),
    FULL_HOUSE: is_full_house,
    FOUR_OF_A_KIND: is_four_of_a_kind,
    LITTLE_STRAIGHT: is_little_straight,
    BIG_STRAIGHT: is_big_straight,
    CHOICE: is_choice,
}


SCORING_FUNCTIONS = {
    YACHT: score_yacht,
    ONES: lambda dice: score_number(dice, 1),
    TWOS: lambda dice: score_number(dice, 2),
    THREES: lambda dice: score_number(dice, 3),
    FOURS: lambda dice: score_number(dice, 4),
    FIVES: lambda dice: score_number(dice, 5),
    SIXES: lambda dice: score_number(dice, 6),
    FULL_HOUSE: score_full_house,
    FOUR_OF_A_KIND: score_four_of_a_kind,
    LITTLE_STRAIGHT: score_little_straight,
    BIG_STRAIGHT: score_big_straight,
    CHOICE: score_choice,
}


# Helper functions

def to_dict(dice):
    d = {}
    for die in dice:
        d[die] = d.get(die, 0) + 1
    return d
