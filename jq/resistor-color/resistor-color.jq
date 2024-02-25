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

if .property == "colors" then colors
elif .property == "colorCode" then .input.color | color_code
end
