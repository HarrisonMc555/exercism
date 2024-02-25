# Given a position {x, y} give the distance from the origin.
def distance_from_origin:
  .x * .x + .y * .y | sqrt;

# Given the distance from the center, return the score.
def score:
  if . <= 1 then 10
  elif . <= 5 then 5
  elif . <= 10 then 1
  else 0 end;

distance_from_origin | score
