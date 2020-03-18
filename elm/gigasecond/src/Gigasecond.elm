module Gigasecond exposing (add)

import Time exposing (Posix, utc)
import Time.Extra exposing (Interval(..))


add : Posix -> Posix
add timestamp =
    Time.Extra.add Second billion utc timestamp


billion : Int
billion =
    1000000000
