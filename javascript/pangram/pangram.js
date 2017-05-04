'use strict';

const Pangram = function Pangram(input) {
  this.text = input;
};

Pangram.prototype.alphabet = (function createAlphabet() {
  const result = [];
  for (let i = 'a'.charCodeAt(0); i <= 'z'.charCodeAt(0); i += 1) {
    result.push(String.fromCharCode(i));
  }
  return result;
}());

Pangram.prototype.strContain = function strContain(s, c) {
  return s.indexOf(c) > -1;
};

Pangram.prototype.isPangram = function isPangram() {
  if (!this.text) { return false; }
  const s = this.text.toLowerCase();
  return this.alphabet.every(c => this.strContain(s, c));
};

module.exports = Pangram;
