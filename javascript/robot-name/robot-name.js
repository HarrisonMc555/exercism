'use strict';

const Robot = function Robot() {
  /* Resets the robot by giving it a new name */
  this.reset = function reset() {
    this.name = this.getNewName();
  };

  /* Create a new name and update set of used names
   This will have issues if there are a large number of robots (more than 100,000) */
  this.getNewName = function getNewName() {
    let newName = this.createName();
    while (Robot.usedNames.has(newName)) {
      newName = this.createName();
    }
    Robot.usedNames.add(newName);
    return newName;
  };

  /* Create a random name (two upper case letters and three numbers) */
  this.createName = function createName() {
    return `${this.randomLetter()}${this.randomLetter()}${this.randomNumber()}${this.randomNumber()}${this.randomNumber()}`;
  };

  /* Create a random upper case letter */
  this.randomLetter = function randomLetter() {
    return String.fromCharCode('A'.charCodeAt(0) + Math.floor(Math.random() * 26));
  };

  /* Create a random digit */
  this.randomNumber = function randomNumber() {
    return Math.floor(Math.random() * 10).toString();
  };

  /* Initialize the robot's name */
  this.name = this.getNewName();
};

/* This is shared set of all used names among all robots */
Robot.usedNames = new Set();

module.exports = Robot;
