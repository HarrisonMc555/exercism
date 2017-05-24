def is_isogram(s):
    sletters = list(filter(str.isalpha, s.lower()))
    return len(set(sletters)) == len(sletters)

def is_isogram_(s):
    seen = set()
    for c in s.lower():
        if not c.isalpha():
            continue
        if c in seen:
            return False
        seen.add(c)
    else:
        return True
