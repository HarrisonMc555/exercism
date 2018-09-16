#include <stdbool.h>
#include <string.h>
#include <ctype.h>

/* This is a O(n^2) algorithm. It could be improved to O(n*log(n)), but that 
   would require implementing a set in C, which I don't want to do right now.
*/

bool is_isogram(const char phrase[]) {
    if (phrase == NULL) {
        return false;
    }
    int i, j, phraselen;
    phraselen = strlen(phrase);
    for (i = 0; i < phraselen; i++) {
        if (!isalpha(phrase[i])) {
            continue;
        }
        for (j = 0; j < i; j++) {
            if (tolower(phrase[i]) == tolower(phrase[j])) {
                return false;
            }
        }
    }
    return true;
}
