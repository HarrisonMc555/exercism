const Triangle = function Triangle(a, b, c) {
  this.kind = (function kindFunc() {
    /* https://stackoverflow.com/a/1063027/7343786 */
    function sortNumber(x, y) {
      return x - y;
    }

    const sides = [a, b, c];
    sides.sort(sortNumber);
    const x = sides[0];
    const y = sides[1];
    const z = sides[2];

    return function kind() {
      if (x <= 0 || x + y <= z) {
        throw Error('Invalid triangle');
      } else if (x === y && y === z) {
        return 'equilateral';
      } else if (x === y || y === z) {
        return 'isosceles';
      }
      return 'scalene';
    };
  }());
};

module.exports = Triangle;
