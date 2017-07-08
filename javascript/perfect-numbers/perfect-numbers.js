// https://www.nayuki.io/res/calculate-divisors-javascript/calculate-divisors.js
function listDivisors(n) {
  if (n < 1) {
    throw new Error('Argument error');
  }
  // Edge case, easier to deal with here
  if (n === 1) {
    return [];
  }
  const small = [];
  const large = [];
  const end = Math.floor(Math.sqrt(n));
  for (let i = 1; i <= end; i += 1) {
    if (n % i === 0) {
      small.push(i);
      // Don't include a square root twice or n
      if (i * i !== n) {
        large.push(n / i);
      }
    }
  }
  // Remove the number itself
  const largeWithoutN = large.slice(1);
  // This is unnecessary, since we don't care about the order.
  // largeWithoutN.reverse();
  return small.concat(largeWithoutN);
}

function sum(arr) {
  return arr.reduce((a, b) => a + b, 0);
}

function PerfectNumbers() {
  this.classify = function classify(n) {
    if (n <= 0) {
      return 'Classification is only possible for natural numbers.';
    }
    const divisorsSum = sum(listDivisors(n));
    if (divisorsSum > n) {
      return 'abundant';
    } else if (divisorsSum < n) {
      return 'deficient';
    }
    return 'perfect';
  };
}

module.exports = PerfectNumbers;
