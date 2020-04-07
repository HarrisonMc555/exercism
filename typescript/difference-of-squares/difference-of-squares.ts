class Squares {
  squareOfSum: number;
  sumOfSquares: number;
  difference: number;
  constructor(num: number) {
    this.squareOfSum = Squares.calcSquareOfSum(num);
    this.sumOfSquares = Squares.calcSumOfSquares(num);
    this.difference = this.squareOfSum - this.sumOfSquares;
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
