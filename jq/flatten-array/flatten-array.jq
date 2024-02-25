def my_flatten:
  if . == null then empty
  elif type == "array" then .[] | my_flatten
  else . end;

[.array | my_flatten]
# .array | (. as [$a] ?// $a | $a)
