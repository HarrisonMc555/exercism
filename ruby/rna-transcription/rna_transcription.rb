class Complement
  DNA_TO_RNA = {
    'G' => 'C',
    'C' => 'G',
    'T' => 'A',
    'A' => 'U'
  }
  def self.dna_to_rna(dna_nucleotide)
    if !DNA_TO_RNA.has_key? dna_nucleotide then
      raise ArgumentError, "Invalid DNA nucleotide"
    end
    DNA_TO_RNA[dna_nucleotide]
  end
  def self.of_dna(rna)
    rna.chars.map { |c| self.dna_to_rna(c) }.join("")
  end
end
