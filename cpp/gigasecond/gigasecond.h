#pragma once
#include <string>
#include "boost/date_time/posix_time/posix_time.hpp"

namespace gigasecond {
    boost::posix_time::ptime advance(boost::posix_time::ptime p);
}
