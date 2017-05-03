const Gigasecond = function Gigasecond(start) {
  this.start = start;
};

Gigasecond.prototype.date = function date() {
  /* 1e9 seconds * 1000 milliseconds/second */
  return new Date(this.start.getTime() + 1e9*1000);
};

module.exports = Gigasecond;
