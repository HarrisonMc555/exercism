class Scrabble {

  private int score;

  Scrabble(String word) {
    this.score = getScore(word);
  }

  int getScore() {
    return this.score;
  }

  static int getScore(String word) {
    word = word.toUpperCase();
    int score = 0;
    for (int i = 0; i < word.length(); i++) {
      char letter = word.charAt(i);
      score += getLetterScore(letter);
    }
    return score;
  }

  static int getLetterScore(char letter) {
    switch (letter) {
      case 'A':
        return 1;
      case 'E':
        return 1;
      case 'I':
        return 1;
      case 'O':
        return 1;
      case 'U':
        return 1;
      case 'L':
        return 1;
      case 'N':
        return 1;
      case 'R':
        return 1;
      case 'S':
        return 1;
      case 'T':
        return 1;
      case 'D':
        return 2;
      case 'G':
        return 2;
      case 'B':
        return 3;
      case 'C':
        return 3;
      case 'M':
        return 3;
      case 'P':
        return 3;
      case 'F':
        return 4;
      case 'H':
        return 4;
      case 'V':
        return 4;
      case 'W':
        return 4;
      case 'Y':
        return 4;
      case 'K':
        return 5;
      case 'J':
        return 8;
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
