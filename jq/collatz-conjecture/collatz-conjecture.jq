def steps:
  if . <= 0 then
    "Only positive integers are allowed" | halt_error
  else
    {"n": ., "steps": 0}
    | until(.n == 1;
            if .n % 2 == 0 then
              .n /= 2
            else
              .n = .n * 3 + 1
            end
             | .steps += 1
           )
    | .steps
  end;
