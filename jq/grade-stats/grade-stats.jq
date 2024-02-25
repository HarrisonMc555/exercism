# Given a numeric score between 0 and 100, output a letter grade
# - "A" is 90% - 100%
# - "B" is 80% - 89%
# - "C" is 70% - 79%
# - "D" is 60% - 69%
# - "F" is  0% - 59%

def letter_thresholds:
  [
    {letter: "A", grade: 90},
    {letter: "B", grade: 80},
    {letter: "C", grade: 70},
    {letter: "D", grade: 60},
    {letter: "F", grade: 0}
  ];

def letter_grade:
  . as $input
  | letter_thresholds
  | map(select($input >= .grade))[0].letter // "F";


# Given an object that maps a student's name to their grade,
# generate an object that maps the letter grade to the number of 
# students with that grade

def count_letter_grades:
  reduce to_entries[] as $item
         (letter_thresholds
           | map({(.letter): 0})
           | add
          ;
          ($item.value | letter_grade) as $grade
          | .[$grade] //= 0
          | .[$grade] += 1
         );
