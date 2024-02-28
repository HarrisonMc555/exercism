def hamming_distance($strand1; $strand2):
  if ($strand1 | length) != ($strand2 | length) then
    "strands must be of equal length" | halt_error
  else
    [range($strand1 | length)
      | select($strand1[.] != $strand2[.])
      | 1]
    | add // 0
  end;
  
hamming_distance(.strand1 / ""; .strand2 / "")
