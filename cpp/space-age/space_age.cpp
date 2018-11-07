#include "space_age.h"

namespace {
    const int SECONDS_PER_MINUTE = 60;
    const int MINUTES_PER_HOUR = 60;
    const int HOURS_PER_DAY = 24;
    const double DAYS_PER_YEAR = 365.25;
    const double SECONDS_PER_EARTH_YEAR = SECONDS_PER_MINUTE * MINUTES_PER_HOUR
                                          * HOURS_PER_DAY * DAYS_PER_YEAR;

    const double MERCURY_YEARS_PER_EARTH_YEAR = 0.2408467;
    const double VENUS_YEARS_PER_EARTH_YEAR = 0.61519726;
    const double MARS_YEARS_PER_EARTH_YEAR = 1.8808158;
    const double JUPITER_YEARS_PER_EARTH_YEAR = 11.862615;
    const double SATURN_YEARS_PER_EARTH_YEAR = 29.447498;
    const double URANUS_YEARS_PER_EARTH_YEAR = 84.016846;
    const double NEPTUNE_YEARS_PER_EARTH_YEAR = 164.79132;
}

namespace space_age {

    space_age::space_age(long age_in_seconds) :
        age_in_seconds(age_in_seconds) {}

    long space_age::seconds() const {
        return this->age_in_seconds;
    }

    double space_age::on_earth() const {
        return this->age_in_seconds / SECONDS_PER_EARTH_YEAR;
    }

    double space_age::on_mercury() const {
        return on_other(MERCURY_YEARS_PER_EARTH_YEAR);
    }

    double space_age::on_venus() const {
        return on_other(VENUS_YEARS_PER_EARTH_YEAR);
    }

    double space_age::on_mars() const {
        return on_other(MARS_YEARS_PER_EARTH_YEAR);
    }

    double space_age::on_jupiter() const {
        return on_other(JUPITER_YEARS_PER_EARTH_YEAR);
    }

    double space_age::on_saturn() const {
        return on_other(SATURN_YEARS_PER_EARTH_YEAR);
    }

    double space_age::on_uranus() const {
        return on_other(URANUS_YEARS_PER_EARTH_YEAR);
    }

    double space_age::on_neptune() const {
        return on_other(NEPTUNE_YEARS_PER_EARTH_YEAR);
    }

    double space_age::on_other(double other_years_per_earth_year) const {
        return this->on_earth() / other_years_per_earth_year;
    }

}
