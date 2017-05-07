'use strict';

const Robot = function Robot() {

  this.reset = function reset() {
    this.name = this.getNewName();
  };

  this.getNewName = function getNewName() {
    let newName = this.createName();
    while (Robot.usedNames.has(newName)) {
      newName = this.createName();
    }
    Robot.usedNames.add(newName);
    return newName;
  };

  this.createName = function createName() {
    return `${this.randomLetter()}${this.randomLetter()}${this.randomNumber()}${this.randomNumber()}${this.randomNumber()}`;
  };

  this.randomLetter = function randomLetter() {
    return String.fromCharCode('A'.charCodeAt(0) + Math.floor(Math.random() * 26));
  };

  this.randomNumber = function randomNumber() {
    return Math.floor(Math.random() * 10).toString();
  };

  this.name = this.getNewName();

};

Robot.usedNames = new Set();

module.exports = Robot;
