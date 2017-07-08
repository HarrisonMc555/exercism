// Package gigasecond contains time operations involving a gigasecond
package gigasecond

import "time"

const testVersion = 4

// AddGigasecond returns the time one gigasecond from t
func AddGigasecond(t time.Time) time.Time {
	return t.Add(gigasecond)
}

const giga = 1000000000
const gigasecond time.Duration = giga * time.Second
