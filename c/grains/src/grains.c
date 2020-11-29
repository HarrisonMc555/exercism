#include "grains.h"

#define MAX_SQUARE 64

uint64_t square(uint8_t index) {
    // Check for out-of-bounds.
    if (index == 0 || index > MAX_SQUARE) {
        return 0;
    }

    // Each square doubles the number of grains. This is essentially 2^n, where
    // n is the 0-based index of the square. As it so happens, raising 2 to a
    // power is equivalent to left shifting a one by a given number because
    // computers use binary. To quote Wikipedia:
    //
    //    Shifting left by n bits on a signed or unsigned binary number has the
    //    effect of multiplying it by 2^n.
    //
    // Since we just want 2^n, we simply take a one and "multiply it by 2^n". We
    // use the "1ULL" syntax to force the one to be an "unsigned long long"
    // integer, i.e. a 64-bit number.
    //
    // See: https://en.wikipedia.org/wiki/Logical_shift

    return 1ULL << (index - 1);
}

uint64_t total(void) {

    // This is a subtle piece of trickery. We want to calculate the sum of all
    // grains from 1 to 64 inclusive. This accounts to the sum of 2^(n-1) with n
    // from 1 to 64 inclusive. Since we're using a 64-bit number, this accounts
    // to having all 64 bits set to a "1".
    //
    // We could write a loop to set every bit from 0 to 63, but we can do
    // better. If we try to subtract 1 from 0 in a 64-bit number, the result
    // will "overflow" and wrap around back to the maximum 64-bit number, which
    // is a number with every bit set to a "1"--exactly what we want! The
    // easiest way to do this is to simply specify the number as -1.
    return -1;
}
