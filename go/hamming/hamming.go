package hamming

import "errors"

const testVersion = 6

func Distance(a, b string) (int, error) {
	if len(a) != len(b) {
		return -1, errors.New("strings not the same length")
	}
	count := 0
	for i := range a {
		if a[i] != b[i] {
			count += 1
		}
	}
	return count, nil
}
