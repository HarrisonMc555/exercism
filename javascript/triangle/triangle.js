function Triangle(a, b, c) {
  /* https://stackoverflow.com/a/1063027/7343786 */
  const [x, y, z] = [a, b, c].sort((i, j) => i - j);

  this.kind = function kind() {
    if (x <= 0 || x + y <= z) {
      throw Error('Invalid triangle');
    } else if (x === y && y === z) {
      return 'equilateral';
    } else if (x === y || y === z) {
      return 'isosceles';
    }
    return 'scalene';
  };
}

module.exports = Triangle;
