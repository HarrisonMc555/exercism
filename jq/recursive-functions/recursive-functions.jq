def debug($item):
  . as $input
  | $item
  | debug
  | $input;

def debug($item; $message):
  . as $input
  | {input: $item, message: $message}
  | debug
  | $input;

# Add the elements of the input array, and return the sum.
#
# Example:
#   [1, 2, 3] | array_add           # => 6

def array_add:
  if length == 0 then
    0
  else
    first + (.[1:] | array_add)
  end;
  # def _array_add_helper($sum):
  #   if length == 0 then
  #     $sum
  #   else
  #     ($sum + first) as $next_sum
  #     | .[1:]
  #     | _array_add_helper($next_sum)
  #   end;
  # _array_add_helper(0);

# Reverse the input array, and return the result in a new array.
#
# Example:
#   [1, 2, 3] | array_reverse       # => [3, 2, 1]

def array_reverse:
  if length == 0 then
    []
  else
    (.[1:] | array_reverse) + [first]
  end;
  # def _array_reverse_helper($reversed):
  #   if length == 0 then
  #     $reversed
  #   else
  #     .[-1] as $last
  #     | .[:-1]
  #     | _array_reverse_helper($reversed + [$last])
  #   end;
  # _array_reverse_helper([]);

# Run the filter `f` for each element of the input array,
# and return the outputs in a new array.
#
# Example:
#   [1, 2, 3] | array_map(. + 1)    # => [2, 3, 4]

def array_map(f):
  if length == 0 then
    []
  else
    [first | f] + (.[1:] | array_map(f))
  end;
  # def _array_map_helper(f; $acc):
  #   if length == 0 then
  #     $acc
  #   else
  #     ($acc + [first | f]) as $acc2
  #     | .[1:]
  #     | _array_map_helper(f; $acc2)
  #   end;
  # _array_map_helper(f; []);
