// Package leap provides utilities for leap years
package leap

const testVersion = 3

// IsLeapYear returns true if year is a leap year
func IsLeapYear(year int) bool {
	isDivisble := func(x, y int) bool {
		return x % y == 0
	}
	return isDivisble(year, 4) && (!isDivisble(year, 100) || isDivisble(year, 400))
}
