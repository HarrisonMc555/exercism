#include "hamming.h"
#include <string>
#include <stdexcept>

int hamming::compute(std::string s1, std::string s2) {
    if (s1.length() != s2.length()) {
        throw std::domain_error("Strings not the same length");
    }
    int diff = 0;
    for (int i=0; i<s1.length(); i++) {
        if (s1.at(i) != s2.at(i)) {
            diff++;
        }
    }
    return diff;
}
