'use strict';

const School = function School() {
  /* Initialize empty object */
  this.students = {};
};

/* Add a student to a particular grade */
School.prototype.add = function add(name, gradeNum) {
  /* Create an empty list for the grade if it doesn't exist yet */
  if (!this.students[gradeNum]) {
    this.students[gradeNum] = [];
  }
  /* Add student and sort
     If you were worried about performance you could do a binary insert here */
  this.students[gradeNum].push(name);
  this.students[gradeNum].sort();
};

/* Return all students */
School.prototype.roster = function roster() {
  /* Return a deep copy */
  return JSON.parse(JSON.stringify(this.students));
};

/* Return all students in a particular grade */
School.prototype.grade = function grade(gradeNum) {
  /* Return a deep copy, default to empty list */
  return JSON.parse(JSON.stringify(this.students[gradeNum] || []));
};

module.exports = School;
