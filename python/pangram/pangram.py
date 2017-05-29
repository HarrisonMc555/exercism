from string import ascii_lowercase


def is_pangram(sentence):
    """Determines if a sentence is a pangram"""
    s = sentence.lower()
    return all([c in s for c in ascii_lowercase])
