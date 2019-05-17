import java.util.Arrays;
import java.util.Random;

class DnDCharacter {
  static Random rand = new Random();
  static final int numRollsForValue = 4;

  private int _strength;
  private int _dexterity;
  private int _constitution;
  private int _intelligence;
  private int _wisdom;
  private int _charisma;
  private int _hitpoints;

  public DnDCharacter() {
    _strength = ability();
    _dexterity = ability();
    _constitution = ability();
    _intelligence = ability();
    _wisdom = ability();
    _charisma = ability();
    _hitpoints = 10 + modifier(_constitution);
  }

  int ability() {
    int[] rolls = new int[numRollsForValue];
    for (int i = 0; i < numRollsForValue; i++) {
      rolls[i] = randomDiceValue();
    }
    Arrays.sort(rolls);
    int smallestRoll = rolls[0];
    return sum(rolls) - smallestRoll;
  }

  int modifier(int input) {
    return divideRoundDown(input - 10, 2);
  }

  int getStrength() {
    return _strength;
  }

  int getDexterity() {
    return _dexterity;
  }

  int getConstitution() {
    return _constitution;
  }

  int getIntelligence() {
    return _intelligence;
  }

  int getWisdom() {
    return _wisdom;
  }

  int getCharisma() {
    return _charisma;
  }

  int getHitpoints() {
    return _hitpoints;
  }

  static int divideRoundDown(int numerator, int denominator) {
    if (denominator >= 0) {
      if (numerator >= 0) {
        return numerator / denominator;
      } else {
        return (numerator + 1) / denominator - 1;
      }
    } else {
      if (numerator > 0) {
        return -1 - (1 - numerator) / denominator;
      } else {
        return numerator / denominator;
      }
    }
  }

  static int randomDiceValue() {
    return 1 + rand.nextInt(6);
  }

  static int sum(int[] array) {
    int _sum = 0;
    for (int x : array) {
      _sum += x;
    }
    return _sum;
  }
}
