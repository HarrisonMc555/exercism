#include "sum_of_multiples.h"

int sum_of_multiples::to(const std::vector<int> factors, int max_num) {
    int sum = 0;
    for (int i = 0; i < max_num; i++) {
        bool is_multiple = false;
        for (int factor : factors) {
            if (i % factor == 0) {
                is_multiple = true;
                break;
            }
        }
        if (is_multiple) {
            sum += i;
        }
    }
    return sum;
}
