def tostring_zpad($num_digits):
  tostring as $in
  | [limit($num_digits; repeat("0"))]
  | add + $in
  | .[-$num_digits:];

def dates_in_month($year; $month):
  ($year | tostring_zpad(4)) as $year_str
  | ($month | tostring_zpad(2)) as $month_str
  | range(31)
  | . + 1
  | tostring_zpad(2) as $date_str
  | "\($year_str)-\($month_str)-\($date_str)"
  | strptime("%Y-%m-%d")
  | mktime
  | select(strftime("%m") == $month_str);

def dates_in_month_strings($year; $month):
  dates_in_month($year; $month)
  | strftime("%Y-%m-%d");

def teenth:
  .[]
  | select(strftime("%d") | tonumber | . >= 13 and . <= 19);
  
def get_day_matching_week($week):
  if $week == "first" then .[0]
  elif $week == "second" then .[1]
  elif $week == "third" then .[2]
  elif $week == "fourth" then .[3]
  elif $week == "last" then .[-1]
  elif $week == "teenth" then teenth
  else error("Invalid week \"\($week)\"")
  end;

. as $info |
  [ dates_in_month($info.year; $info.month)
    | select(strftime("%A") == $info.dayofweek)
  ]
  | get_day_matching_week($info.week)
  | strftime("%Y-%m-%d")
