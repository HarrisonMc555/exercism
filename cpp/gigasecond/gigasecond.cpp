#include <string>
#include "gigasecond.h"
#include "boost/date_time/posix_time/posix_time.hpp"

using namespace boost::posix_time;

ptime gigasecond::advance(ptime p) {
    return p + seconds(1000000000L);
}
