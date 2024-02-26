def encode:
  def encode_char($char; $count):
    if $count > 1
    then ($count | tostring) + $char
    else $char
    end;

  def helper($acc; $prev; $count):
    if length == 0 then
      $acc + encode_char($prev; $count)
    else
      .[0] as $c
      | .[1:]
      | if $c == $prev then
          helper($acc; $prev; $count + 1)
        else
          helper($acc + encode_char($prev; $count); $c; 1)
        end
    end;
  split("") | helper(""; ""; 0);

def replicate($count; $sequence):
  [limit($count; repeat($sequence))];

def decode:
  def decode_char($char; $count):
    if $count == 0 then
      $char
    else
      replicate($count; $char) | add
    end;
  
  def helper($acc; $count):
    if length == 0 then
      if $count > 0 then
        error("invalid run length encoding, unterminated count of \($count)")
      else
        $acc
      end
    else
      first as $c
      | .[1:]
      | if ($c | test("\\d")) then
          ($c | tonumber) as $digit
          | ($count * 10 + $digit) as $new_count
          | helper($acc; $new_count)
        else
          helper($acc + decode_char($c; $count); 0)
        end
    end;
  split("") | helper(""; 0);
