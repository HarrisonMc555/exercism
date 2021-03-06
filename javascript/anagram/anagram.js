const Anagram = function Anagram(subject) {
  /* Store the original word and the letters */
  this.subject = subject;
  this.letters = this.toLetters(subject);
};

const _ = require('underscore');

/* Return an array of anagram matches */
Anagram.prototype.matches = function matches(...rest) {
  const first = rest[0];
  let words;
  /* If the argument is an array, use as is, else spread the arguments */
  if (first.constructor === Array) {
    words = first;
  } else {
    words = rest;
  }
  return words.filter(this.isAnagram.bind(this));
};

/* Returns true if the given word is an anagram of the objects subject */
Anagram.prototype.isAnagram = function isAnagram(word) {
  /* Must have the same letters but not be the same word */
  return _.isEqual(this.letters, this.toLetters(word)) &&
         word.toLowerCase() !== this.subject;
};

/* Extracts a sorted array of letters from a string */
Anagram.prototype.toLetters = function toLetters(word) {
  return word.toLowerCase().split('').sort();
};

module.exports = Anagram;
