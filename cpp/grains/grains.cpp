#include "grains.h"

u64 grains::square(u32 square_index) {
    // We need 2^(square_index - 1)
    // Left shift is a quick and easy way to do that
    return 1ULL << (square_index - 1);
}

u64 grains::total() {
    // One way to think about this is that each "square" on the chess board,
    // when added to our total, will set one bit of our answer. Since there are
    // 64 bits, we want all 64 bits of our result to be set. An easy way to set
    // all bits is to negate 0.
    return ~0ULL;
}
