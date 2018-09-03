# Produce the RNA complement to a given DNA strand
class Complement
  def self.of_dna(rna)
    rna.chars.map { |c| dna_to_rna(c) }.join('')
  end

  private_class_method def self.dna_to_rna(dna_nucleotide)
    unless DNA_TO_RNA.key? dna_nucleotide
      raise ArgumentError, 'Invalid DNA nucleotide'
    end
    DNA_TO_RNA[dna_nucleotide]
  end

  DNA_TO_RNA = {
    'G' => 'C',
    'C' => 'G',
    'T' => 'A',
    'A' => 'U'
  }.freeze
end
