public class Twofer {
    public String twofer(String name) {
        String whom = isBlank(name) ? "you" : name;
        return "One for " + whom + ", one for me.";
    }

    private static boolean isBlank(String string) {
        return string == null || "".equals(string);
    }
}
