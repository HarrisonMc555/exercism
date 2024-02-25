def init_sieve:
  . as $limit
  | [limit($limit + 1; repeat(true))]
;

def update_sieve($factor; $limit):
  . as $sieve
  | [ 2 * $factor | while (. <= $limit; . += $factor)]
  | reduce .[] as $number ($sieve; .[$number] |= false)
;

def sieve:
  . as $limit
  | init_sieve as $sieve
  | [range(2; $limit + 1)]
  | reduce .[] as $factor
    ( $sieve;
      if .[$factor]
      then update_sieve($factor; $limit)
      else .
      end
    )
  | to_entries
  | map(if .key >= 2 and .value then .key else empty end)
;

.limit | sieve
