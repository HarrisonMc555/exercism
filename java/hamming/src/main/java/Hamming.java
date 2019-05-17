import java.util.stream.IntStream;

class Hamming {

  private String leftStrand, rightStrand;
  private int distance;

  Hamming(String leftStrand, String rightStrand) {
    if (leftStrand.length() != rightStrand.length()) {
      throw new IllegalArgumentException("leftStrand and rightStrand must be of equal length.");
    }
    this.leftStrand = leftStrand;
    this.rightStrand = rightStrand;
    this.distance =
        (int)
            IntStream.range(0, leftStrand.length())
                .filter(i -> leftStrand.charAt(i) != rightStrand.charAt(i))
                .count();
  }

  int getHammingDistance() {
    return distance;
  }
}
