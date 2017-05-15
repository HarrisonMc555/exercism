'use strict';

const ETL = function ETL() {};

ETL.prototype.transform = function transform(oldScores) {
  let result = {};
  for (let score in oldScores) {
    /* This check is to make eslint happy. I could tell eslint to ignore it, but
     * this is just for reference right now.
     */
    if (ETL.prototype.hasOwnProperty.call(oldScores, score)) {
      for (let i = 0; i < oldScores[score].length; i++) {
        let letter = oldScores[score][i];
        result[letter.toLowerCase()] = Number.parseInt(score, 10);
      }
    }
  }
  return result;
};

module.exports = ETL;
