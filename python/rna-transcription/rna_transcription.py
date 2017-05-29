CONVERSION = {'G': 'C',
              'C': 'G',
              'T': 'A',
              'A': 'U'}


def convert_base(b):
    """Returns RNA base complement for DNA base."""
    return CONVERSION[b]


def to_rna(dna_sequence):
    """Returns RNA complement of DNA sequence."""
    try:
        return ''.join(map(convert_base, dna_sequence))
    except KeyError:
        return ''
