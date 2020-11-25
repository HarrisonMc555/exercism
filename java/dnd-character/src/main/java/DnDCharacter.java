import java.util.Arrays;
import java.util.Random;

class DnDCharacter {
  static Random rand = new Random();
  static final int numRollsForValue = 4;

  private int strength;
  private int dexterity;
  private int constitution;
  private int intelligence;
  private int wisdom;
  private int charisma;
  private int hitpoints;

  public DnDCharacter() {
    strength = ability();
    dexterity = ability();
    constitution = ability();
    intelligence = ability();
    wisdom = ability();
    charisma = ability();
    hitpoints = 10 + modifier(constitution);
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
    // return (input - 10) / 2;
  }

  int getStrength() {
    return strength;
  }

  int getDexterity() {
    return dexterity;
  }

  int getConstitution() {
    return constitution;
  }

  int getIntelligence() {
    return intelligence;
  }

  int getWisdom() {
    return wisdom;
  }

  int getCharisma() {
    return charisma;
  }

  int getHitpoints() {
    return hitpoints;
  }

  static final int divideRoundDown(int dividend, int divisor) {
    int quotient = dividend / divisor;
    boolean dividendIsNegative = dividend < 0;
    boolean divisorIsNegative = divisor < 0;
    boolean quotientIsNegative = dividendIsNegative ^ divisorIsNegative;
    if (quotientIsNegative && quotient * divisor != dividend) {
      return quotient - 1;
    } else {
      return quotient;
    }
  }

  static int randomDiceValue() {
    return 1 + rand.nextInt(6);
  }

  static int sum(int[] array) {
    int sum = 0;
    for (int x : array) {
      sum += x;
    }
    return sum;
  }
}
