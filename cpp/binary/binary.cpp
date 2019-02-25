#include "binary.h"

#include <string>

unsigned int binary::convert(std::string input) {
    unsigned int result = 0;

    for (char digit : input) {
        // Could use left shift here, but it will end up as the same code
        // anyways and this is more clear
        result *= 2;

        if (digit == '0') {
            // Add nothing
        } else if (digit == '1') {
            result += 1;
        } else {
            // Error, non-binary digit
            return 0;
        }
    }

    return result;
}
