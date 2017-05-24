"""
Exercism.io pangram kata
"""

from string import ascii_lowercase


def valid_letter(letter):
    """Checks if letter is in the ascii alphabet"""
    return letter in ascii_lowercase


def get_unique_letters(sentence):
    """Returns a set of letters used in a sentence"""
    return set(filter(valid_letter, list(sentence.lower())))


def is_pangram(sentence):
    """Determines if a sentence is a pangram"""
    return 26 is len(get_unique_letters(sentence))
