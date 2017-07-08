/* eslint no-prototype-builtins: "off" */

const XRegExp = require('xregexp');

function Words() {
  this.count = function count(str) {
    // https://stackoverflow.com/a/14912552
    const lowerStr = str.toLocaleLowerCase();
    // Match everything that's not a letter, number, or single quote (')
    const nonLetters = XRegExp("[^\\p{L}\\pN']", 'g');
    const cleanStr = XRegExp.replace(lowerStr, nonLetters, ' ');
    const words = cleanStr.match(/\S+/g) || [];
    const counts = {};
    for (let i = 0; i < words.length; i += 1) {
      const uncleanWord = words[i];
      const match = uncleanWord.match(/^'(.*)'/);
      let word = uncleanWord;
      if (match) {
        // Just the first matched part, see
        // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_function_as_a_parameter
        word = match[1];
      }

      if (counts.hasOwnProperty(word)) {
        counts[word] += 1;
      } else {
        counts[word] = 1;
      }
    }
    return counts;
  };
}

module.exports = Words;
