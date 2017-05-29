#include <stdio.h>
#include <iostream>
#include "sieve.h"

namespace sieve {
    /* Returns all primes below max using Eratosthenes sieve */
    std::vector<int> primes(int max) {
        std::vector<int> primes = std::vector<int>();
        bool erat[max];
        for (int i = 0; i < max; i++) {
            erat[i] = false;
        }
        for (int n = 2; n < max; n++) {
            if (erat[n]) {
                continue;
            }
            primes.push_back(n);
            for (int i = 2*n; i < max; i += n) {
                erat[i] = true;
            }
        }
        return primes;
    }
};
