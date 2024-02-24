.number as $number
  | [{"factor": 3, "sound": "Pling"},
     {"factor": 5, "sound": "Plang"},
     {"factor": 7, "sound": "Plong"}]
  | map(if $number % .factor == 0 then .sound else empty end)
  | if length == 0 then $number else add end
