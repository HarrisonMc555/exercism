def init_bases:
  "ACGT" / "" | map({(.): 0}) | add;

reduce (.strand / "" | .[]) as $base (
         init_bases;
         if has($base) then
           .[$base] += 1
         else
           "Invalid nucleotide in strand" | halt_error
         end
)
