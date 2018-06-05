# Decoding

def decode(string):
    parts = []
    while string:
        part, rest = decode_part(string)
        parts.append(part)
        string = rest
    return ''.join(parts)


def decode_part(string):
    if not string:
        raise Exception('Cannot decode part of an empty string')
    letter_or_number = string[0]
    if letter_or_number.isdigit():
        digits, rest = split_by(string, str.isdigit)
        n = int(''.join(digits))
        assert rest
        letter, actual_rest = rest[0], rest[1:]
        part = letter * n
        return part, actual_rest
    else:
        letter = string[0]
        actual_rest = string[1:]
        return string[0], actual_rest


# Encoding

def encode(string):
    parts = []
    while string:
        c, n, rest = encode_part(string)
        parts.append((c, n))
        string = rest
    return ''.join(encoding_part_to_string(c, n) for c, n in parts)


def encode_part(string):
    if not string:
        raise Exception('Cannot encode part of an empty string')
    letter = string[0]
    beg, rest = split_by(string, lambda c: c == letter)
    return letter, len(beg), rest


def encoding_part_to_string(letter, num):
    if num == 1:
        return letter
    return str(num) + letter


# Helper functions

def split_by(l, f):
    beg = []
    for v in l:
        if f(v):
            beg.append(v)
        else:
            break
    rest = l[len(beg):]
    return beg, rest
