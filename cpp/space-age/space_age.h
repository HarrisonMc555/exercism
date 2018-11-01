#ifndef SPACE_AGE_H
#define SPACE_AGE_H

namespace space_age {

    class space_age {
    private:
        long age_in_seconds;

    public:
        explicit space_age(long age_in_seconds);
        long seconds() const;
        double on_earth() const;
        double on_mercury() const;
        double on_venus() const;
        double on_mars() const;
        double on_jupiter() const;
        double on_saturn() const;
        double on_uranus() const;
        double on_neptune() const;

    private:
        double on_other(double other_years_per_earth_year) const;
    };
}

#endif
