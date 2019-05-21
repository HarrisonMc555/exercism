import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

class ProteinTranslator {

  private static final Map<String, String> rnaCodonToProtein;
  private static final Set<String> rnaStopCodons;

  static {
    rnaCodonToProtein = new HashMap<String, String>();
    rnaCodonToProtein.put("AUG", "Methionine");
    rnaCodonToProtein.put("UUU", "Phenylalanine");
    rnaCodonToProtein.put("UUC", "Phenylalanine");
    rnaCodonToProtein.put("UUA", "Leucine");
    rnaCodonToProtein.put("UUG", "Leucine");
    rnaCodonToProtein.put("UCU", "Serine");
    rnaCodonToProtein.put("UCC", "Serine");
    rnaCodonToProtein.put("UCA", "Serine");
    rnaCodonToProtein.put("UCG", "Serine");
    rnaCodonToProtein.put("UAU", "Tyrosine");
    rnaCodonToProtein.put("UAC", "Tyrosine");
    rnaCodonToProtein.put("UGU", "Cysteine");
    rnaCodonToProtein.put("UGC", "Cysteine");
    rnaCodonToProtein.put("UGG", "Tryptophan");

    rnaStopCodons = new HashSet<String>();
    rnaStopCodons.add("UAA");
    rnaStopCodons.add("UAG");
    rnaStopCodons.add("UGA");
  }

  List<String> translate(String rnaSequence) {
    if (rnaSequence.length() % 3 != 0) {
      throw new IllegalArgumentException(
          String.format("Invalid rna sequence length %d", rnaSequence.length()));
    }
    List<String> proteins = new ArrayList<String>();
    for (int i = 0; i < rnaSequence.length(); i += 3) {
      String rnaCodon = rnaSequence.substring(i, i + 3);
      if (isRnaStopCodon(rnaCodon)) {
        break;
      }
      String protein = rnaCodonToProtein.get(rnaCodon);
      if (protein == null) {
        throw new IllegalArgumentException(String.format("Invalid rna codon %s", rnaCodon));
      }
      proteins.add(protein);
    }
    return proteins;
  }

  private static boolean isRnaStopCodon(String rnaCodon) {
    return rnaStopCodons.contains(rnaCodon);
  }
}
