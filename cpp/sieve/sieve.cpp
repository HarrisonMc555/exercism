#include <stdio.h>
#include <iostream>
#include "sieve.h"

namespace sieve {
    std::vector<int> primes(int max) {
        // cout << "primes(" << max << ")" << endl;
        std::vector<int> primes = std::vector<int>();
        bool erat[max];
        for (int i = 0; i < max; i++) { erat[i] = false; }
        for (int n = 2; n < max; n++) {
            // cout << "\tn:" << n << endl;
            if (erat[n]) {
                // cout << "\t\t--not " << n << endl;
                continue;
            }
            primes.push_back(n);
            for (int i = 2*n; i < max; i += n) {
                // cout << "\t\ti:" << i << endl;
                erat[i] = true;
            }
        }
        return primes;
    }
};
