module Gigasecond exposing (add)

import Time
import Time.Extra


add : Time.Posix -> Time.Posix
add timestamp =
    Time.Extra.add Time.Extra.Second billion Time.utc timestamp


billion : Int
billion =
    1000000000
