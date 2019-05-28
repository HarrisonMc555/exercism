import java.util.stream.IntStream;

class Scrabble {

  private int score;

  Scrabble(String word) {
    this.score = getScore(word);
  }

  int getScore() {
    return this.score;
  }

  static int getScore(String word) {
    String upperCaseWord = word.toUpperCase();
    return IntStream.range(0, word.length())
        .map(i -> getLetterScore(upperCaseWord.charAt(i)))
        .sum();
  }

  static int getLetterScore(char letter) {
    switch (letter) {
      case 'A':
      case 'E':
      case 'I':
      case 'O':
      case 'U':
      case 'L':
      case 'N':
      case 'R':
      case 'S':
      case 'T':
        return 1;
      case 'D':
      case 'G':
        return 2;
      case 'B':
      case 'C':
      case 'M':
      case 'P':
        return 3;
      case 'F':
      case 'H':
      case 'V':
      case 'W':
      case 'Y':
        return 4;
      case 'K':
        return 5;
      case 'J':
      case 'X':
        return 8;
      case 'Q':
        return 10;
      case 'Z':
        return 10;
      default:
        throw new IllegalArgumentException("Illegal letter " + letter);
    }
  }
}
