import itertools

firsts = set('abc')
seconds = set(range(4))
DEBUG = 0

def get_mappings3(firsts, seconds):
    if len(seconds) < len(firsts):
        return
    for permutation in itertools.permutations(seconds, len(firsts)):
        yield dict(zip(firsts, permutation))

def get_mappings2(firsts, seconds):
    if len(seconds) < len(firsts):
        return
    yield from get_mappings2_helper(firsts, 0, seconds, 0, {})
    

def get_mappings2_helper(firsts, first_index, seconds, second_index, mapping):
    if first_index >= len(firsts) or second_index >= len(seconds):
        yield mapping
        return
    for i in range(first_index, len(firsts)):
        next_i = i + 1
        first = firsts[i]
        for j in range(second_index, len(seconds)):
            next_j = j + 1
            second = seconds[j]
            mapping[first] = second
            yield from get_mappings2_helper(firsts, next_i, seconds, next_j,
                                            mapping)
            del mapping[first]
        


def get_mappings(firsts, seconds):
    # for mapping in get_mappings_helper(firsts, seconds, {}):
    #     yield mapping
    yield from get_mappings_helper(firsts, seconds, {})


def get_mappings_helper(firsts, seconds, mapping):
    global DEBUG
    print('\t'*DEBUG, 'firsts:', sorted(firsts), 'seconds:', sorted(seconds), 'mapping:', mapping)
    if not firsts:
        # print('\t'*(DEBUG + 1), 'Bottom out')
        yield mapping
        return
    # print('\t'*DEBUG, 'NOT bottom out')
    for first in list(sorted(firsts)):
        firsts.remove(first)
        for second in list(sorted(seconds)):
            seconds.remove(second)
            mapping[first] = second
            DEBUG += 1
            yield from get_mappings_helper(firsts, seconds, mapping)
            # for mapping_result in get_mappings_helper(firsts, seconds, mapping):
            #     # print('yielding result')
            #     yield mapping_result
            DEBUG -= 1
            del mapping[first]
            seconds.add(second)
        firsts.add(first)


for mapping in get_mappings3(list('ab'), list(range(4))):
    ss = []
    for key in sorted(mapping):
        ss.append(str(key) + str(mapping[key]))
    print(' '.join(ss))
    # print('RESULT:', mapping)
    # print(mapping)
