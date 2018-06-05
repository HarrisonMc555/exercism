import string


def hey(phrase):
    if is_yelling_question(phrase):
        return 'Calm down, I know what I\'m doing!'
    elif is_question(phrase):
        return 'Sure.'
    elif is_yelling(phrase):
        return 'Whoa, chill out!'
    elif is_saying_nothing(phrase):
        return 'Fine. Be that way!'
    else:
        return 'Whatever.'


def is_question(phrase):
    phrase = phrase.strip()
    if not phrase:
        return False
    return phrase[-1] == '?'


def is_yelling(phrase):
    phrase = phrase.strip()
    if not any(letter in phrase for letter in string.ascii_letters):
        return False
    return not any(letter in phrase for letter in string.ascii_lowercase)


def is_yelling_question(phrase):
    return is_question(phrase) and is_yelling(phrase)


def is_saying_nothing(phrase):
    return not phrase.strip()
