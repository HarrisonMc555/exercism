import java.util.HashSet;
import java.util.Set;

public class PangramChecker {
  private static final String alphabet = "abcdefghijklmnopqrstuvwxyz";
  private static final int alphabetLength = alphabet.length();

  public boolean isPangram(String input) {
    Set<Character> includesChars = new HashSet<Character>();
    for (char letter : input.toLowerCase().toCharArray()) {
      if (!Character.isLetter(letter)) {
        continue;
      }

      if (!includesChars.contains(letter)) {
        includesChars.add(letter);
        if (includesChars.size() == alphabetLength) {
          return true;
        }
      }
    }
    return false;
  }
}
