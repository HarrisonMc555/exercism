#include <vector>
#include <cmath>
#include <algorithm>

#include <iostream>

#include "prime_factors.h"

typedef unsigned int uint;

std::vector<uint> prime_sieve(uint max_num);

namespace prime_factors {
    std::vector<int> of(uint number) {
        uint max_factor = std::sqrt(number);
        std::vector<uint> primes = prime_sieve(max_factor);
        std::vector<int> factors;

        for (uint prime : primes) {
            while (number % prime == 0) {
                factors.push_back(prime);
                number /= prime;
            }
        }

        // Check if the starting number was prime
        if (number != 1) {
            factors.push_back(number);
        }

        std::sort(factors.begin(), factors.end());

        return factors;
    }
}

std::vector<uint> prime_sieve(uint max_num) {
    std::vector<bool> sieve(max_num + 1, false);
    std::vector<uint> primes;

    for (uint num = 2; num <= max_num; num++) {
        uint is_composite = sieve.at(num);
        if (is_composite) continue;
        primes.push_back(num);
        for (uint multiple = num*2; multiple < max_num; multiple += num) {
            sieve.at(multiple) = true;
        }
    }

    return primes;
}
