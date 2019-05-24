class Darts {

  private Point dartPoint;
  private int _score;

  static final Point center = new Point(0, 0);
  static final double outerRadius = 10, middleRadius = 5, innerRadius = 1;
  static final int noTargetPoints = 0, outerPoints = 1, middlePoints = 5, innerPoints = 10;

  Darts(double x, double y) {
    this.dartPoint = new Point(x, y);
    this._score = calculateScore(dartPoint);
  }

  int score() {
    return _score;
  }

  static int calculateScore(Point dartPoint) {
    double dartDistance = center.distanceTo(dartPoint);
    if (dartDistance <= innerRadius) {
      return innerPoints;
    } else if (dartDistance <= middleRadius) {
      return middlePoints;
    } else if (dartDistance <= outerRadius) {
      return outerPoints;
    } else {
      return noTargetPoints;
    }
  }

  public static class Point {
    private double x, y;

    public Point(double x, double y) {
      this.x = x;
      this.y = y;
    }

    public double distanceTo(Point other) {
      double dx = this.x - other.x;
      double dy = this.y - other.y;
      return Math.sqrt(dx * dx + dy * dy);
    }
  }
}
