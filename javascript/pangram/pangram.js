'use strict';

const Pangram = function Pangram(input) {
  this.text = input;
};

/* Return true if the Pangram object is a pangram, i.e. contains every letter in
   the alphabet */
Pangram.prototype.isPangram = function isPangram() {
  if (!this.text) { return false; }
  const s = this.text.toLowerCase();
  return this.alphabet.every(c => this.strContain(s, c));
};

/* Create an array with all lower case letters */
Pangram.prototype.alphabet = (function createAlphabet() {
  let result = [];
  for (let i = 'a'.charCodeAt(0); i <= 'z'.charCodeAt(0); i += 1) {
    result.push(String.fromCharCode(i));
  }
  return result;
}());

/* Return true if s contains c */
Pangram.prototype.strContain = function strContain(s, c) {
  return s.indexOf(c) > -1;
};

module.exports = Pangram;
