const MINUTES_PER_HOUR: i32 = 60;
const HOURS_PER_DAY: i32 = 24;
const MINUTES_PER_DAY: i32 = MINUTES_PER_HOUR * HOURS_PER_DAY;

#[derive(PartialEq, PartialOrd, Debug)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let hours = rem(hours, HOURS_PER_DAY);
        let minutes = rem(minutes, MINUTES_PER_DAY);
        let total_minutes = hours * MINUTES_PER_HOUR + minutes;
        let hours = total_minutes / MINUTES_PER_HOUR % HOURS_PER_DAY;
        let minutes = total_minutes % MINUTES_PER_HOUR;
        Clock {
            hours,
            minutes
        }
    }

    pub fn add_minutes(self, minutes: i32) -> Self {
        Clock::new(self.hours, self.minutes + minutes)
    }
}

impl std::fmt::Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:02}:{:02}", self.hours, self.minutes)
    }
}

fn rem(number: i32, divisor: i32) -> i32 {
    ((number % divisor) + divisor) % divisor
}
