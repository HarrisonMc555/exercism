const Triangle = function Triangle(a, b, c) {
  this.kind = (function kindFunc() {
    /* https://stackoverflow.com/a/1063027/7343786 */
    function sortNumber(x, y) {
      return x - y;
    }

    var sides = [a, b, c];
    sides.sort(sortNumber);
    let x = sides[0];
    let y = sides[1];
    let z = sides[2];

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
  })();
};

module.exports = Triangle;
