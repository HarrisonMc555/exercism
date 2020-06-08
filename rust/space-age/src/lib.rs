// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

/// A duration of time with second precision
#[derive(Debug)]
pub struct Duration(u64);

impl Duration {
    const SECONDS_PER_EARTH_YEAR: u64 = 31557600;

    pub fn in_earth_years(&self) -> f64 {
        self.0 as f64 / Self::SECONDS_PER_EARTH_YEAR as f64
    }
}

impl From<u64> for Duration {
    /// This converts a u64 number of seconds into a Duration object
    fn from(s: u64) -> Self {
        Duration(s)
    }
}

pub trait Planet {
    /// The orbital period of the planet in Earth years
    fn earth_years_per_planet_year() -> f64;

    /// Years on this planet during the given duration
    fn years_during(d: &Duration) -> f64 {
        d.in_earth_years() / Self::earth_years_per_planet_year()
    }
}

pub struct Mercury;
pub struct Venus;
pub struct Earth;
pub struct Mars;
pub struct Jupiter;
pub struct Saturn;
pub struct Uranus;
pub struct Neptune;

impl Planet for Mercury {
    fn earth_years_per_planet_year() -> f64 {
        0.2408467
    }
}

impl Planet for Venus {
    fn earth_years_per_planet_year() -> f64 {
        0.61519726
    }
}

impl Planet for Earth {
    fn earth_years_per_planet_year() -> f64 {
        1.0
    }
}

impl Planet for Mars {
    fn earth_years_per_planet_year() -> f64 {
        1.8808158
    }
}

impl Planet for Jupiter {
    fn earth_years_per_planet_year() -> f64 {
        11.862615
    }
}

impl Planet for Saturn {
    fn earth_years_per_planet_year() -> f64 {
        29.447498
    }
}

impl Planet for Uranus {
    fn earth_years_per_planet_year() -> f64 {
        84.016846
    }
}

impl Planet for Neptune {
    fn earth_years_per_planet_year() -> f64 {
        164.79132
    }
}
