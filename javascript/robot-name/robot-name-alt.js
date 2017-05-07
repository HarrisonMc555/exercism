'use strict';

const Robot = function Robot() {
  var usedNames = new Set();
  var unusedNames = new Set(this.allPossibleNames);
  this.createName = function createName() {return 0;};
  var name = this.createName();

  this.allCapitalLetters = (function createAllCapitalLetters() {
    let result = [];
    for (let i = 'A'.charCodeAt(0); i <= 'Z'.charCodeAt(0); i += 1) {
      result.push(String.fromCharCode(i));
    }
    console.log(result);
    return result;
  })();

  this.allNumbers = (function createAllNumbers() {
    let result = [];
    for (let i = 0; i < 10; i += 1) {
      result.push(i.toString());
    }
    console.log(result);
    return result;
  })();

  this.allPossibleNames = (function createAllPossibleNames() {
    let result = [];
    console.log('allPossibleNames');
    console.log(this.toString());
    for (let ci1 = 0; ci1 < this.allCapitalLetters.length; ci1++) {
      for (let ci2 = 0; ci2 < this.allCapitalLetters.length; ci2++) {
        for (let ci3 = 0; ci3 < this.allCapitalLetters.length; ci3++) {
          for (let ni1 = 0; ni1 < this.allNumbers.length; ni1++) {
            for (let ni2 = 0; ni2 < this.allNumbers.length; ni2++) {
              console.log(`${c1}`);
              result.push(`${c1}${c2}${c3}${n1}${n2}`);
            }
          }
        }
      }
    }
    console.log(result);
    return result;
  }).bind(this)();

  this.createName = function createName() {
    return 'BAD11';
  };

  console.log(this.allCapitalLetters);
  console.log(this.allNumbers);
};


module.exports = Robot;

let robot = new Robot();
