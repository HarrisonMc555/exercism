def diff(pair):
    """Returns 0 when pair elements are equal, otherwise 1"""
    x, y = pair
    return 0 if x == y else 1


def distance(strand_a, strand_b):
    """Returns hamming distance of two dna strands"""
    if len(strand_a) != len(strand_b):
        raise ValueError("Strands must be of the same length")
    return sum(map(diff, zip(strand_a, strand_b)))
