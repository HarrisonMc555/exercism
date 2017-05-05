const BeerSong = function BeerSong() {};

const capitalizeFirstLetter = function capitalizeFirstLetter(s) {
  return s.charAt(0).toUpperCase() + s.slice(1);
};

BeerSong.prototype.verse = function verse(n) {
  var first = n;
  var second = (n - 1 + 100) % 100;
  var firstNum;
  var firstBottle;
  var takeOrGet;
  var secondNum;
  var secondBottle;
  /* Numbers */
  if (first === 0) {
    firstNum = 'no more';
  } else {
    firstNum = first.toString();
  }
  if (second === 0) {
    secondNum = 'no more';
  } else {
    secondNum = second.toString();
  }
  /* Bottles */
  if (first === 1) {
    firstBottle = 'bottle';
  } else {
    firstBottle = 'bottles';
  }
  if (second === 1) {
    secondBottle = 'bottle';
  } else {
    secondBottle = 'bottles';
  }
  /* What's next? Take some down or get some more */
  if (first === 1) {
    takeOrGet = 'Take it down and pass it around';
  } else if (first === 0) {
    takeOrGet = 'Go to the store and buy some more';
  } else {
    takeOrGet = 'Take one down and pass it around';
  }
  /* Combine results */
  return `${capitalizeFirstLetter(firstNum)} ${firstBottle} of beer on the wall, ${firstNum} ${firstBottle} of beer.\n${takeOrGet}, ${secondNum} ${secondBottle} of beer on the wall.\n`;
};

BeerSong.prototype.sing = function sing(a, b) {
  var aa = a;
  const bb = b || 0;
  var verses = [];
  /* Loop through the verses */
  while (aa >= bb) {
    verses.push(BeerSong.prototype.verse(aa));
    aa -= 1;
  }
  return verses.join('\n');
};

module.exports = BeerSong;
