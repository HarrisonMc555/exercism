// 'use strict';

const bigInt = require('big-integer');

const Grains = function Grains() {
  this.square = function square(n) {
    return bigInt(2).pow(n - 1).toString();
  };

  this.total = (function createTotalFunc() {
    let totalGrains = bigInt(2).pow(64).minus(1).toString();
    return function total() {
      return totalGrains;
    };
  })();
};

module.exports = Grains;
