import java.util.LinkedHashMap;
import java.util.Map;

class ResistorColor {
  public static final Map<String, Integer> colorToCode =
      new LinkedHashMap<String, Integer>() {
        {
          put("black", 0);
          put("brown", 1);
          put("red", 2);
          put("orange", 3);
          put("yellow", 4);
          put("green", 5);
          put("blue", 6);
          put("violet", 7);
          put("grey", 8);
          put("white", 9);
        }
      };

  int colorCode(String color) {
    if (!colorToCode.containsKey(color)) {
      throw new IllegalArgumentException("invalid color");
    }
    return colorToCode.get(color);
  }

  String[] colors() {
    return colorToCode.keySet().toArray(new String[colorToCode.size()]);
  }
}
