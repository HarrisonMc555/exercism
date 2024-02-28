def my_reverse:
  if type == "string" then
    split("") | my_reverse | add // ""
  elif type == "array" then
    . as $input
    | [range(length - 1; -1; -1) | $input[.]]
  else
    error("cannot reverse \(type)")
  end;

.value | my_reverse
