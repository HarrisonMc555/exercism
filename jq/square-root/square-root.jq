def average_int:
  length as $length
  | add
  | . / $length
  | floor;

def square($x):
  $x * $x;

def my_sqrt:
  if . <= 0 then error("Non-natural input \(.)") end
  | . as $x
  | [1, .]
  | until(. as [$min, $max] | $max <= $min;
          . as [$min, $max]
           | average_int as $root
           | if square($root) <= $x and square($root + 1) > $x then
               [$root, $root]
             elif $root * $root > $x then
               [$min, $root - 1]
             else
               [$root + 1, $max]
             end
         )
  | .[0];

.radicand | my_sqrt
