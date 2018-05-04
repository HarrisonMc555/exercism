import re


quote_pattern = re.compile("^'.*'$")
def strip_quotes(word):
    if quote_pattern.match(word):
        return word[1:-1]
    else:
        return word


def word_count(phrase):
    pattern = re.compile("[^a-zA-Z0-9']")
    phrase = pattern.sub(' ', phrase)
    phrase = phrase.lower()
    words = phrase.split()
    words = [strip_quotes(w) for w in words]
    d = {}
    for word in words:
        if word not in d:
            d[word] = 1
        else:
            d[word] += 1
    return d
