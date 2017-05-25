"""
Exercism.io Hamming kata
"""


def validate_dna_strands(strand_a, strand_b):
    """Raises ValueError if strands are not valid"""
    if len(strand_a) is not len(strand_b):
        raise ValueError("Strands must be of the same length")


def distance(strand_a, strand_b):
    """Returns hamming distance of two dna strands"""
    validate_dna_strands(strand_a, strand_b)

    count = 0

    for index, nucleotide in enumerate(strand_a):
        if nucleotide is not strand_b[index]:
            count += 1

    return count