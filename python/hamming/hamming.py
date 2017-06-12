def diff(pair):
    """Returns True when pair elements are , otherwise False."""
    x, y = pair
    return x != y


def distance(strand_a, strand_b):
    """Returns hamming distance of two dna strands."""
    if len(strand_a) != len(strand_b):
        raise ValueError("Strands must be of the same length")
    return len(filter(diff, zip(strand_a, strand_b)))
