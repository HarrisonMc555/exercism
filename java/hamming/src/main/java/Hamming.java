import java.util.stream.IntStream;

class Hamming {

  private String leftStrand, rightStrand;

  Hamming(String leftStrand, String rightStrand) {
    if (leftStrand.length() != rightStrand.length()) {
      throw new IllegalArgumentException("leftStrand and rightStrand must be of equal length.");
    }
    this.leftStrand = leftStrand;
    this.rightStrand = rightStrand;
  }

  int getHammingDistance() {
    int num_differences = 0;
    return (int)
        IntStream.range(0, leftStrand.length())
            .filter(i -> leftStrand.charAt(i) != rightStrand.charAt(i))
            .count();
  }
}
