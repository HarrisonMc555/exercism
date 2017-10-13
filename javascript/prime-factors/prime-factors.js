// https://stackoverflow.com/a/12287599/7343786
function getPrimes(max) {
  const sieve = [];
  const primes = [];
  for (let i = 2; i <= max; i += 1) {
    if (!sieve[i]) {
      // i has not been marked -- it is prime
      primes.push(i);
      for (let j = i * 2; j <= max; j += i) {
        sieve[j] = true;
      }
    }
  }
  return primes;
}

function isDivisibleBy(n, divisor) {
  return n % divisor === 0;
}

function getPrimeFactors(n) {
  const largestFactor = Math.floor(Math.sqrt(n));
  const primes = getPrimes(largestFactor);
  const factors = [];
  let currentNumber = n;
  for (let i = 0; i < primes.length; i += 1) {
    const prime = primes[i];
    while (isDivisibleBy(currentNumber, prime)) {
      factors.push(prime);
      currentNumber /= prime;
    }
    if (currentNumber === 1) {
      break;
    }
  }
  if (currentNumber !== 1) {
    factors.push(currentNumber);
  }
  return factors;
}

const primeFactors = {
  for: getPrimeFactors,
};

// For some reason this doesn't work
// const primeFactors = function primeFactors() {
//   this.for = getPrimeFactors;
// };

module.exports = primeFactors;
