var FoodChain = require('./food-chain');

var song = new FoodChain();

/* for (let i = 1; i <= 8; i++) {
 *   console.log(i);
 *   console.log(song.verse(i))p;
 * }*/

console.log(song.verses(1,4));
