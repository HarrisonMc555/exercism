"""
Exercism.io RNA Kata
"""

CONVERSION = [('G', 'C'),
              ('C', 'G'),
              ('T', 'A'),
              ('A', 'U')]


def convert_nucleotide(nucleotide):
    """Converts DNA nucleotide to RNA"""
    for complement in CONVERSION:
        if complement[0] == nucleotide:
            return complement[1]


def validate_dna_sequence(dna_sequence):
    """Validates a DNA sequence"""
    is_valid = True

    if not dna_sequence:
        return False

    for nucleotide in dna_sequence:
        if nucleotide not in dict(CONVERSION):
            return False

    return is_valid


def to_rna(dna_sequence):
    """Returns RNA complement of DNA sequence"""
    if validate_dna_sequence(dna_sequence):
        return ''.join(map(convert_nucleotide, list(dna_sequence)))
    else:
        return ''