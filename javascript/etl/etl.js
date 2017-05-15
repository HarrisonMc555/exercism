'use strict';

const ETL = function ETL(input) {};

ETL.prototype.transform = function transform(oldScores) {
  let result = {};
  for (let score in oldScores) {
    for (let i = 0; i < oldScores[score].length; i++) {
      let letter = oldScores[score][i];
      result[letter.toLowerCase()] = Number.parseInt(score);
    }
  }
  return result;
};

module.exports = ETL;
