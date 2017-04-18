//
// This is only a SKELETON file for the "Leap" exercise. It's been provided as a
// convenience to get you started writing code faster.
//

const Year = function Year(input) {
  this.year = input;
};

Year.prototype.isLeap = function isLeap() {
  return ((this.year % 4 === 0)
       && !(this.year % 100 === 0))
      || (this.year % 400 === 0);
};

module.exports = Year;
