const Isogram = function Isogram(word) {
  this.word = word;
};

/* Return true if an array has duplicates */
function hasDuplicates(array) {
  /* Convert to set (no duplicates) and compare sizes */
  return (new Set(array)).size !== array.length;
}

/* Create a regex that matches all non-letters, treating unicode characters
   correctly */
let xRegExp = require('xregexp');
/* Use global flag to match all */
const allNonlettersRegexp = xRegExp('\\PL', 'g');

/* Return true if the word is an isogram (no repeated letters) */
Isogram.prototype.isIsogram = function isIsogram() {
  /* Convert to lower case, remove all non-letter characters, and check to see
     if there are no duplicates */
  return !hasDuplicates(xRegExp.replace(this.word.toLowerCase(),
                                        allNonlettersRegexp, '').split(''));
};

module.exports = Isogram;
