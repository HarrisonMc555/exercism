def colors:
  [ "black"
  , "brown"
  , "red"
  , "orange"
  , "yellow"
  , "green"
  , "blue"
  , "violet"
  , "grey"
  , "white"
  ];

def color_code:
  . as $color
  | colors
  | to_entries
  | map({key: .value, value: .key})
  | from_entries
  | .[$color];

def from_digits:
  reduce .[] as $digit (0; . * 10 + $digit);

[limit(2; .colors[]) | color_code] | from_digits
