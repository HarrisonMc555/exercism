class Dna {
  val nucleotideCounts: Map<Char, Int>

  constructor(strand: String) {
    nucleotideCounts = getNucleotideCounts(strand)
  }

  private fun getNucleotideCounts(strand: String): Map<Char, Int> {
    val empty: MutableMap<Char, Int> = mutableMapOf('A' to 0, 'C' to 0, 'G' to 0, 'T' to 0)
    val increment: (MutableMap<Char, Int>, Char) -> MutableMap<Char, Int> = { acc, nucleotide ->
      val count = acc.get(nucleotide)
      if (count == null) {
        throw IllegalArgumentException("Invalid nucleotide $nucleotide")
      }
      acc.put(nucleotide, count + 1)
      acc
    }
    return strand.fold(empty, increment)
  }
}
