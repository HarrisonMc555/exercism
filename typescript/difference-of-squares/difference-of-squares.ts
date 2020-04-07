class Squares {
  private _squareOfSum: number;
  private _sumOfSquares: number;
  private _difference: number;
  constructor(num: number) {
    this._squareOfSum = Squares.calcSquareOfSum(num);
    this._sumOfSquares = Squares.calcSumOfSquares(num);
    this._difference = this._squareOfSum - this._sumOfSquares;
  }

  get squareOfSum(): number {
    return this._squareOfSum;
  }

  get sumOfSquares(): number {
    return this._sumOfSquares;
  }

  get difference(): number {
    return this._difference;
  }

  static calcSquareOfSum(num: number): number {
    const sum = (num * (num + 1)) / 2;
    return sum * sum;
  }

  static calcSumOfSquares(num: number): number {
    return (num * (num + 1) * (2 * num + 1)) / 6;
  }
}

export default Squares;
