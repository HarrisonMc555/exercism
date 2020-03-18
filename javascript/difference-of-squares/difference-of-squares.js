export class Squares {
  constructor(n) {
    this._sumOfSquares = calcSumOfSquares(n);
    this._squareOfSum = calcSquareOfSum(n);
    this._difference = this._squareOfSum - this._sumOfSquares;
  }

  get sumOfSquares() {
    return this._sumOfSquares;
  }

  get squareOfSum() {
    return this._squareOfSum;
  }

  get difference() {
    return this._difference;
  }
}

const calcSumOfSquares = (n) => {
  return n * (n + 1) * (2 * n + 1) / 6;
};

const calcSquareOfSum = (n) => {
  let sum = n * (n + 1) / 2;
  return sum * sum;
};
