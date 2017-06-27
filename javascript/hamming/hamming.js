const Hamming = function Hamming() {};

Hamming.prototype.compute = function compute(seqA, seqB) {
  if (seqA.length !== seqB.length) {
    throw Error('DNA strands must be of equal length.');
  }
  return Array.prototype.filter.call(seqA, (e, i) => e !== seqB[i]).length;
};

module.exports = Hamming;
