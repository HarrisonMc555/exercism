#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>

bool add_to_set32(uint32_t* set, int x) {
    if (x < 0 || x >= 32) {
        return false;
    }
    (*set) |= (1 << x);
    return true;
}

bool set32_contains(uint32_t* set, int x) {
    if (x < 0 || x >= 32) {
        return false;
    }
    return (*set) & (1 << x);
}

int char_index(char c) {
    if ('a' <= c && c <= 'z') {
        return c - 'a';
    }
    if ('A' <= c && c <= 'Z') {
        return c - 'A';
    }
    return -1;
}

bool is_isogram(const char phrase[]) {
    if (phrase == NULL) {
        return false;
    }
    uint32_t set = 0;
    int i, phraselen;
    phraselen = strlen(phrase);
    for (i = 0; i < phraselen; i++) {
        char c = tolower(phrase[i]);
        if (!isalpha(c)) {
            continue;
        }
        int c_index = char_index(c);
        if (set32_contains(&set, c_index)) {
            return false;
        }
        add_to_set32(&set, c_index);
    }
    return true;
}
