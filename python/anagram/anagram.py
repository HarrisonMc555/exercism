def get_letters(word):
    return ''.join(sorted(word.lower()))


def same_letters(w1, w2):
    return get_letters(w1) == get_letters(w2) and not \
        w1.lower() == w2.lower()


def detect_anagrams(word, candidates):
    return [candidate for candidate in candidates
            if same_letters(word, candidate)]
