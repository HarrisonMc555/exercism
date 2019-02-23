#include "sum_of_multiples.h"
#include <unordered_set>

int sum_of_multiples::to(const std::vector<int> factors, int max_num) {
    std::unordered_set<int> multiples;

    for (int factor : factors) {
        int multiple = factor;
        while (multiple < max_num) {
            multiples.insert(multiple);
            multiple += factor;
        }
    }

    int sum = 0;
    for (int multiple : multiples) {
        sum += multiple;
    }

    return sum;
}
