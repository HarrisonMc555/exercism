#include "armstrong_numbers.h"
#include <math.h>

#define BASE 10

static int pow_int(const int base, const int power) {
    int result = 1;
    for (int i = 0; i < power; i++) {
        result *= base;
    }
    return result;
}

int is_armstrong_number(const int candidate) {
    int candidate_copy = candidate;
    int sum = 0;
    const int num_digits = log10(candidate) + 1;
    while (candidate_copy > 0) {
        const int digit = candidate_copy % BASE;
        sum += pow_int(digit, num_digits);
        candidate_copy /= BASE;
    }
    return candidate == sum;
}
