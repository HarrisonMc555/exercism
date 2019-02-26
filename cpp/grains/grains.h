#ifndef GRAINS_H
#define GRAINS_H

#include <cstdint>

namespace grains {
    constexpr uint64_t square(uint32_t square_index) {
        return 1ULL << (square_index - 1);
    }

    constexpr uint64_t total() {
        uint64_t result = 0ULL;
        const uint32_t BOARD_WITH = 8;
        const uint32_t NUM_SQUARES = BOARD_WITH * BOARD_WITH;
        for (uint32_t i = 0; i < NUM_SQUARES; i++) {
            result += square(i);
        }
        return result;
    }
}

#endif
