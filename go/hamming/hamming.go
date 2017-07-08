// Package hamming provides functions for hamming DNA calculations
package hamming

import "errors"

const testVersion = 6

// Distance calculates the hamming distance between DNA strands a and b
// This is the total number of places that the DNA strands differ. Note that the
// strands must be equal in length
func Distance(a, b string) (int, error) {
	if len(a) != len(b) {
		return -1, errors.New("strings not the same length")
	}
	count := 0
	for i := range a {
		if a[i] != b[i] {
			count++
		}
	}
	return count, nil
}
