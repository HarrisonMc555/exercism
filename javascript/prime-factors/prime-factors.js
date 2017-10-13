// https://stackoverflow.com/a/12287599/7343786
function getPrimes(max) {
  var sieve = [], i, j, primes = [];
  for (i = 2; i <= max; ++i) {
    if (!sieve[i]) {
      // i has not been marked -- it is prime
      primes.push(i);
      for (j = i << 1; j <= max; j += i) {
        sieve[j] = true;
      }
    }
  }
  return primes;
}

function isDivisibleBy(n, divisor) {
  return n % divisor == 0;
}

function getPrimeFactors(n) {
  const largestFactor = Math.floor(Math.sqrt(n));
  const primes = getPrimes(largestFactor);
  let factors = [];
  let i = 0;
  for (let i = 0; i < primes.length; i += 1) {
    const prime = primes[i];
    while (isDivisibleBy(n, prime)) {
      factors.push(prime);
      n /= prime;
    }
    if (n == 1) {
      break;
    }
  }
  if (n != 1) {
    factors.push(n);
  }
  return factors;
}

const primeFactors = {
  "for": getPrimeFactors
};

// For some reason this doesn't work
// const primeFactors = function primeFactors() {
//   this.for = getPrimeFactors;
// };

module.exports = primeFactors;
