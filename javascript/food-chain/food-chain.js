'use strict';

const FoodChain = function FoodChain() {};

/* Text for the verses
   [<animal>, <comment>[, <animal to catch>]] */
FoodChain.prototype.text = [
  ['fly', 'I don\'t know why she swallowed the fly. Perhaps she\'ll die.'],
  ['spider', 'It wriggled and jiggled and tickled inside her.', 'spider that wriggled and jiggled and tickled inside her'],
  ['bird', 'How absurd to swallow a bird!'],
  ['cat', 'Imagine that, to swallow a cat!'],
  ['dog', 'What a hog, to swallow a dog!'],
  ['goat', 'Just opened her throat and swallowed a goat!'],
  ['cow', 'I don\'t know how she swallowed a cow!'],
  ['horse', 'She\'s dead, of course!']
];

/* Return string for a particular verse */
FoodChain.prototype.verse = function verse(n) {
  let first = n - 1;
  let result = [];
  /* Start with swallowing first animal and comment */
  result.push(`I know an old lady who swallowed a ${this.text[first][0]}.`);
  result.push(this.text[first][1]);
  /* If this is the last verse, skip next part */
  if (first < this.text.length - 1) {
    /* Keep swallowing other animals */
    for (let i = first - 1; i >= 0; i--) {
      result.push(`She swallowed the ${this.text[i + 1][0]} to catch the ${this.text[i][2] || this.text[i][0]}.`);
    }
    /* Ending statement */
    if (first > 0) {
      result.push('I don\'t know why she swallowed the fly. Perhaps she\'ll die.');
    }
  }
  return result.join('\n') + '\n';
};

/* Return the string for a sequence of verses */
FoodChain.prototype.verses = function verses(first, last) {
  let results = [];
  for (let i = first; i <= last; i++) {
    results.push(this.verse(i));
  }
  return results.join('\n') + '\n';
};

module.exports = FoodChain;
