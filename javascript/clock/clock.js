const leftPad = require('left-pad');

const minutesPerHour = 60;
const hoursPerDay = 24;
const minutesPerDay = minutesPerHour * hoursPerDay;
const maxMinutes = minutesPerDay;

const mod = function mod(x, d) {
  let rem = x % d;
  if (rem < 0) {
    rem += d;
  }
  return rem;
};

const calcMinutes = function calcMinutes(hours, minutes) {
  const realMinutes = minutes || 0;
  return (hours * minutesPerHour) + realMinutes;
};

const getMinutes = function getMinutes(totalMinutes) {
  return totalMinutes % minutesPerHour;
};

const getHours = function getHours(totalMinutes) {
  return Math.floor(totalMinutes / minutesPerHour);
};

const clockToString = function clockToString(totalMinutes) {
  const minutes = getMinutes(totalMinutes);
  const hours = getHours(totalMinutes);
  const minutesStr = leftPad(minutes, 2, '0');
  const hoursStr = leftPad(hours, 2, '0');
  return `${hoursStr}:${minutesStr}`;
};

const Clock = {
  at: function at(hours, minutes) {
    const totalMinutes = mod(calcMinutes(hours, minutes), maxMinutes);

    this._totalMinutes = totalMinutes;

    this.toString = function toString() {
      return clockToString(totalMinutes);
    };

    this.equals = function equals(other) {
      /* If we try to use this._totalMinutes === other._totalMinutes, we
         run into problems with `this` ... */
      return totalMinutes === other._totalMinutes;
    };

    this.plus = function plus(diffMinutes) {
      return at(0, totalMinutes + diffMinutes);
    };

    this.minus = function minus(diffMinutes) {
      return at(0, totalMinutes - diffMinutes);
    };

    return this;
  },

};

module.exports = Clock;
