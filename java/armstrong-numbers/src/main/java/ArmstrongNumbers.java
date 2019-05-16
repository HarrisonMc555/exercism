import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

class ArmstrongNumbers {

  boolean isArmstrongNumber(int numberToCheck) {
    List<Integer> digits = digits(numberToCheck);
    int numDigits = digits.size();
    int armstrongSum = IntStream.range(0, numDigits).map(i -> pow(digits.get(i), numDigits)).sum();
    return numberToCheck == armstrongSum;
  }

  List<Integer> digits(int number) {
    ArrayList<Integer> digits = new ArrayList<Integer>();
    while (number > 0) {
      digits.add(number % 10);
      number /= 10;
    }
    Collections.reverse(digits);
    return digits;
  }

  int pow(int base, int exponent) {
    return IntStream.range(0, exponent).map(_i -> base).reduce((a, b) -> a * b).getAsInt();
  }
}
