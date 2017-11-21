import re
# import string

def word_count(phrase):
    pattern = re.compile("[^a-zA-Z0-9']")
    phrase = pattern.sub(' ', phrase)
    phrase = phrase.lower()
    words = phrase.split()
    # words = [w for w in words if is_word(w)]
    d = {}
    for word in words:
        if word not in d:
            d[word] = 1
        else:
            d[word] += 1
    return d


# def is_word(word):
#     return any(c in word for c in string.ascii_letters)

