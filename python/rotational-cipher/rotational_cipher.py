import string


ALPHABET_LENGTH = len(string.ascii_lowercase)


def rotate(text, key):
    return ''.join(rotate_char(c, key) for c in text)


def rotate_char(c, n):
    if c.islower():
        c = rotate_lowercase(c, n)
    elif c.isupper():
        c = rotate_uppercase(c, n)
    return c


def index_lowercase(c):
    assert c.islower()
    return ord(c) - ord('a')


def index_uppercase(c):
    assert c.isupper()
    return ord(c) - ord('A')


def rotate_lowercase(c, n):
    c_index = index_lowercase(c)
    new_c_index = (c_index + n) % ALPHABET_LENGTH
    new_c = chr(ord('a') + new_c_index)
    return new_c


def rotate_uppercase(c, n):
    c_index = index_uppercase(c)
    new_c_index = (c_index + n) % ALPHABET_LENGTH
    new_c = chr(ord('A') + new_c_index)
    return new_c
