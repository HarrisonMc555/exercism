def toRna:
  def rnaComplement:
    {"G": "C", "C": "G", "T": "A", "A": "U"}[.];
  split("") | map(rnaComplement) | join("");
