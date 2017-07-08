const ETL = function ETL() {};

ETL.prototype.transform = function transform(oldScores) {
  const result = {};
  const scores = Object.keys(oldScores);
  for (let i = 0; i < scores.length; i += 1) {
    const score = scores[i];
    const letters = oldScores[score];
    for (let j = 0; j < letters.length; j += 1) {
      const letter = letters[j];
      result[letter.toLowerCase()] = Number.parseInt(score, 10);
    }
  }
  return result;
};

module.exports = ETL;
