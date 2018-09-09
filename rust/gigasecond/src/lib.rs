extern crate chrono;
use chrono::{DateTime, Duration, Utc};

// Returns a Utc DateTime one billion seconds after start.
pub fn after(start: DateTime<Utc>) -> DateTime<Utc> {
    let giga = (10 as i64).pow(9);
    let offset = Duration::seconds(giga);
    start + offset
}
