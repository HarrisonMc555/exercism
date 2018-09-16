#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h> /* Debugging */

char *next_word(char *phrase, bool isfirstword);

char *abbreviate(const char *phrase) {
    if (phrase == NULL) {
        return NULL;
    }
    int phraselen = strlen(phrase);
    char phrasecopy[phraselen];
    /* We don't know how much space we'll need for the acronym. If we allocate
       enough space to fit the entire phrase, we know we'll have enough space.
    */
    char *acronym = malloc(phraselen);
    char *curphrase = phrasecopy; /* Set to first letter */
    int index = 0;
    bool isfirstword = true;
    /* This is just so that we don't ignore the 'const' modifier of the input.
     */
    strcpy(phrasecopy, phrase);
    while (true) {
        curphrase = next_word(curphrase, isfirstword);
        if (curphrase == NULL) {
            break;
        }
        acronym[index++] = toupper(curphrase[0]); /* Add first letter */
        isfirstword = false;
    }
    if (strlen(acronym) == 0) {
        return NULL;
    }
    return acronym;
}

char *next_word(char *phrase, bool isfirstword) {
    if (phrase == NULL) {
        return NULL;
    }
    int phraselen = strlen(phrase);
    /* If we start the entire phrase inside the first word, we want to return
       this as the "next" word. After that point, we should always keep going
       until we exit the current word first, then return the point where we find
       the NEXT word. */
    bool outsidecurword = isfirstword;
    for (int i = 0; i < phraselen; i++) {
        char c = phrase[i];
        if (outsidecurword && isalpha(c)) {
            /* Found the first letter of the next word. Move the char *
               pointer forward by that many chars. */
            return phrase + i;
        } else if (!outsidecurword && !isalpha(c) && c != '\'') {
            /* This branch tests if we've found the end of the current word.
               A single apostrophe doesn't make you leave the current word
               (possesive words or contractions). */
            outsidecurword = true;
        }
    }
    return NULL;
}
