# frozen_string_literal: true

# Calculate hamming distance between DNA sequences
class Hamming
  def self.compute(seq1, seq2)
    if seq1.length != seq2.length
      raise ArgumentError, 'DNA sequences must be the same length'
    end

    seq1.chars.zip(seq2.chars).count { |c1, c2| c1 != c2 }
  end
end
