def bob:
  sub("\\s*$"; "")
  | if test("^\\s*$") then
      "Fine. Be that way!"
    else
      test("[a-z]") as $has_lower_case
      | test("[A-Z]") as $has_upper_case
      | ($has_upper_case and ($has_lower_case | not)) as $is_yelling
      | . / ""
      | (last == "?") as $is_question
      | if $is_yelling then
          if $is_question then
            "Calm down, I know what I'm doing!"
          else
            "Whoa, chill out!"
          end
        else
          if $is_question then
            "Sure."
          else
            "Whatever."
          end
        end
    end
;

.heyBob | bob
            # .heyBob | . as $input | bob | $input + " -> " + .
