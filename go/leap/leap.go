// Package leap provides utilities for leap years
package leap

const testVersion = 3

// IsLeapYear returns true if year is a leap year
func IsLeapYear(year int) bool {
	multiple := func(x int) bool {
		return divisibleBy(year, x)
	}
	return multiple(4) && (!multiple(100) || multiple(400))
}

func divisibleBy(x, y int) bool {
	return x%y == 0
}
