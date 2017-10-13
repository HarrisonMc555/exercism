function binaryStringToDecimal(binaryString) {
  const binaryStringPattern = /^[01]+$/;
  if (!binaryStringPattern.test(binaryString)) {
    return 0;
  }
  const base = 2;
  let totalNumber = 0;
  let currentPower = 1;
  for (let i = binaryString.length - 1; i >= 0; i -= 1) {
    totalNumber += parseInt(binaryString[i], 10) * currentPower;
    currentPower *= base;
  }
  return totalNumber;
}

const Binary = function Binary(binaryString) {
  this.binaryString = binaryString;
  this.toDecimal = function toDecimal() {
    return binaryStringToDecimal(this.binaryString);
  };
};

module.exports = Binary;
