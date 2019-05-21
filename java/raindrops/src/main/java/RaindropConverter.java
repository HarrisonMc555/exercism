class RaindropConverter {

  String convert(int number) {
    StringBuilder soundBuilder = new StringBuilder();
    if (isDivisbleBy(number, 3)) soundBuilder.append("Pling");
    if (isDivisbleBy(number, 5)) soundBuilder.append("Plang");
    if (isDivisbleBy(number, 7)) soundBuilder.append("Plong");
    String sounds = soundBuilder.toString();
    if (sounds.length() == 0) return Integer.toString(number);
    else return sounds;
  }

  private static boolean isDivisbleBy(int number, int dividend) {
    return number % dividend == 0;
  }
}
