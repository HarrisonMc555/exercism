// Package clock keeps track of minutes and hours
package clock

import "fmt"

const testVersion = 4

// Clock represents a minute/hour combination
type Clock int

// New creates a Clock from an hour and minute value
func New(hour, minute int) Clock {
	return fromMinute(hour*minutesPerHour + minute)
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.hour(), c.minute())
}

// Add increases the minute value stored in a Clock
func (c Clock) Add(minute int) Clock {
	return fromMinute(int(c) + minute)
}

func (c Clock) minute() int {
	return int(c) % minutesPerHour
}

func (c Clock) hour() int {
	return int(c) / minutesPerHour
}

func fromMinute(minute int) Clock {
	return Clock(mod(minute, totalMinute))
}

func mod(x, d int) int {
	rem := x % d
	if rem < 0 {
		rem += d
	}
	return rem
}

const minutesPerHour int = 60
const maxHour = 24

const totalMinute = minutesPerHour * maxHour
