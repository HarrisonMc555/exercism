class Hamming
  def self.compute(seq1, seq2)
    if seq1.length != seq2.length then
      raise ArgumentError, 'DNA sequences must be the same length'
    end
    seq1.split('').zip(seq2.split('')).count { |c1, c2| c1 != c2 }
  end
end

