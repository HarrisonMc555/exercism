#include "hamming.h"

#define NULL 0

int compute(const char *lhs, const char *rhs) {
    if (lhs == NULL && rhs == NULL) {
        return 0;
    } else if (lhs == NULL || rhs == NULL) {
        return -1;
    }
    int num_diff = 0;
    for (unsigned int i = 0;; i++) {
        char lc = lhs[i];
        char rc = rhs[i];
        if (lc == 0 && rc == 0) {
            return num_diff;
        } else if (lc == 0 || rc == 0) {
            return -1;
        }
        if (lc != rc) {
            num_diff++;
        }
    }
}
