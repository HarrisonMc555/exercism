def has_duplicates:
  length != (unique | length);

def construct:
  if (.preorder | length) != (.inorder | length) then
    "traversals must have the same length"
    | halt_error
  elif (.preorder | sort) != (.inorder | sort) then
    "traversals must have the same elements"
    | halt_error
  elif (.preorder | has_duplicates) or (.inorder | has_duplicates) then
    "traversals must contain unique items"
    | halt_error
  elif (.preorder | length) == 0 then
    {}
  else
    .preorder[0] as $root
    | (.inorder | index($root)) as $root_index
    | .inorder[:$root_index] as $inorder_left
    | .inorder[$root_index + 1:] as $inorder_right
    | (.preorder - [$root]) as $preorder
    | ($preorder - $inorder_right) as $preorder_left
    | ($preorder - $inorder_left) as $preorder_right
    | {"preorder": $preorder_left, "inorder": $inorder_left}
    | construct as $left
    | {"preorder": $preorder_right, "inorder": $inorder_right}
    | construct as $right
    | {"v": $root, "l": $left, "r": $right}
  end
;

construct
