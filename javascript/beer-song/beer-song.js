/* eslint quotes: ["off"] */

const BeerSong = function BeerSong() {};

/* Use pluralize library */
let pluralize = require('pluralize');
pluralize.addSingularRule(/^one$/i, 'it');
pluralize.addPluralRule(/^one$/i, 'one');

/* Capitalize the first letter */
BeerSong.prototype.capitalizeFirstLetter = function capitalizeFirstLetter(s) {
  return s.charAt(0).toUpperCase() + s.slice(1);
};

/* Pluralize if n > 1 */
BeerSong.prototype.possiblyPluralize = function possiblyPluralize(s, n) {
  if (n === 1) {
    return pluralize.singular(s);
  }
  return pluralize.plural(s);
};

/* Get the string for a number or 'no more' if it's 0 */
BeerSong.prototype.getNumString = function getNumString(n) {
  if (n === 0) {
    return 'no more';
  }
  return n.toString();
};

/* Get the closing line of the beer song for the number */
BeerSong.prototype.takeOrGet = function takeOrGet(n) {
  if (n === 0) {
    return 'Go to the store and buy some more';
  }
  return `Take ${this.possiblyPluralize('one', n)} down and pass it around`;
};

/* Get a verse of the beer song */
BeerSong.prototype.verse = function verse(n) {
  const first = n;
  const second = (n - 1 + 100) % 100;
  const firstNum = this.getNumString(first);
  const secondNum = this.getNumString(second);
  const firstBottle = this.possiblyPluralize('bottle', first);
  const secondBottle = this.possiblyPluralize('bottle', second);
  return `${this.capitalizeFirstLetter(firstNum)} ${firstBottle} ` +
    `of beer on the wall, ${firstNum} ${firstBottle} of beer.\n` +
    `${this.takeOrGet(first)}, ${secondNum} ${secondBottle} ` +
    `of beer on the wall.\n`;
};

/* Sing a range of verses, default to going to the end */
BeerSong.prototype.sing = function sing(a, b) {
  let aa = a;
  const bb = b || 0;
  let verses = [];
  /* Loop through the verses */
  while (aa >= bb) {
    verses.push(this.verse(aa));
    aa -= 1;
  }
  return verses.join('\n');
};

module.exports = BeerSong;
