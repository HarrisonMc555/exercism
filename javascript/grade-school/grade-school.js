const School = function School() {
  /* Initialize empty object */
  var students = {};

  /* Add a student to a particular grade */
  this.add = function add(name, gradeNum) {
    /* Create an empty list for the grade if it doesn't exist yet */
    if (!students[gradeNum]) {
      students[gradeNum] = [];
    }
    /* Add student and sort
       If you were worried about performance you could do a binary insert here */
    students[gradeNum].push(name);
    students[gradeNum].sort();
  };

  /* Return all students */
  this.roster = function roster() {
    /* Return a deep copy */
    return JSON.parse(JSON.stringify(students));
  };

  /* Return all students in a particular grade */
  this.grade = function grade(gradeNum) {
    /* Return a deep copy, default to empty list */
    return JSON.parse(JSON.stringify(students[gradeNum] || []));
  };
};

module.exports = School;

/* Additional test cases:

it('Shouldn\'t be able to internally access the database', function() {
  school.add('Bob', 5);
  school.students = { 4 : [ 'Hacked' ] };
  var expectedDb = { 5 : [ 'Bob' ] };
  expect(school.roster()).toEqual(expectedDb);
});

it('Shouldn\'t be able to override functions', function() {
  School.prototype.add = function hackedAdd(name, gradeNum) {
    let hackedName = `Hacked ${name}`;
    let hackedGradeNum = gradeNum + 1;
    if (!this.students[hackedGradeNum]) {
      this.students[hackedGradeNum] = [];
    }
    this.students[hackedGradeNum].push(hackedName);
    this.students[hackedGradeNum].sort();
  };
  school = new School();
  school.add('Mary', 9);
  var expectedDb = { 9 : [ 'Mary' ] };
  expect(school.roster()).toEqual(expectedDb);
});

*/
