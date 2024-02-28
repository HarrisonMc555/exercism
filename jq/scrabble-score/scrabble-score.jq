def letters_and_score:
  [
    [["A", "E", "I", "O", "U", "L", "N", "R", "S", "T"], 1],
    [["D", "G"], 2],
    [["B", "C", "M", "P"], 3],
    [["F", "H", "V", "W", "Y"], 4],
    [["K"], 5],
    [["J", "X"], 8],
    [["Q", "Z"], 10]
  ];

def letter_to_score:
  letters_and_score
  | map(. as [$letters, $score]
         | $letters
         | map({(.): $score})
         | add
       )
  | add;

letter_to_score as $score_map
  | .word
  | ascii_upcase
  | . / ""
  | map(letter_to_score[.])
  | add // 0
