#include <stdio.h>
#include <iostream>
#include "sieve.h"

using namespace std;

vector<int> primes(int max) {
    // cout << "primes(" << max << ")" << endl;
    vector<int> primes = vector<int>();
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

void printPrimes(vector<int> primes) {
    for (int x : primes) {
        cout << x << ", ";
    }
}

int main() {
    vector<int> p10 = primes(10);
    cout << "primes(10): ";
    printPrimes(p10);
    vector<int> p1000 = primes(1000);
    cout << "primes(1000): ";
    printPrimes(p1000);
}
