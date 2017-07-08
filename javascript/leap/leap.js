const Year = function Year(input) {
  this.year = input;
};

Year.prototype.isLeap = function isLeap() {
  return ((this.year % 4 === 0)
       && !(this.year % 100 === 0))
      || (this.year % 400 === 0);
};

module.exports = Year;
