import java.util.HashMap;
import java.util.Map;

class Scrabble {

  private String word;
  private int score;

  static final Map<Character, Integer> letterToScore;

  static {
    letterToScore = new HashMap<Character, Integer>();
    letterToScore.put('A', 1);
    letterToScore.put('E', 1);
    letterToScore.put('I', 1);
    letterToScore.put('O', 1);
    letterToScore.put('U', 1);
    letterToScore.put('L', 1);
    letterToScore.put('N', 1);
    letterToScore.put('R', 1);
    letterToScore.put('S', 1);
    letterToScore.put('T', 1);
    letterToScore.put('D', 2);
    letterToScore.put('G', 2);
    letterToScore.put('B', 3);
    letterToScore.put('C', 3);
    letterToScore.put('M', 3);
    letterToScore.put('P', 3);
    letterToScore.put('F', 4);
    letterToScore.put('H', 4);
    letterToScore.put('V', 4);
    letterToScore.put('W', 4);
    letterToScore.put('Y', 4);
    letterToScore.put('K', 5);
    letterToScore.put('J', 8);
    letterToScore.put('X', 8);
    letterToScore.put('Q', 10);
    letterToScore.put('Z', 10);
  }

  Scrabble(String word) {
    this.word = word;
    this.score = getScore(this.word);
  }

  int getScore() {
    return this.score;
  }

  static int getScore(String word) {
    word = word.toUpperCase();
    int score = 0;
    for (int i = 0; i < word.length(); i++) {
      char c = word.charAt(i);
      score += letterToScore.get(c);
    }
    return score;
  }
}
