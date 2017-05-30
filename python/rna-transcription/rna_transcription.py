CONVERSION = {'G': 'C',
              'C': 'G',
              'T': 'A',
              'A': 'U'}


def to_rna(dna_sequence):
    """Returns RNA complement of DNA sequence."""
    try:
        return ''.join(map(CONVERSION.__getitem__, dna_sequence))
    except KeyError:
        return ''
