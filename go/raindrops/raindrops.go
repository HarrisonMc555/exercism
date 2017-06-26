// Package raindrops makes the sound of raindrops for some numbers
package raindrops

import (
	"strconv"
	"strings"
)

const testVersion = 3

// Convert transforms a number into raindrop sounds..or a number string
func Convert(x int) string {
	d3 := divisibleBy(x, 3)
	d5 := divisibleBy(x, 5)
	d7 := divisibleBy(x, 7)
	if !(d3 || d5 || d7) {
		return strconv.Itoa(x)
	}
	drops := make([]string, 0)
	if d3 {
		drops = append(drops, "Pling")
	}
	if d5 {
		drops = append(drops, "Plang")
	}
	if d7 {
		drops = append(drops, "Plong")
	}
	return strings.Join(drops, "")
}

func divisibleBy(x, y int) bool {
	return x%y == 0
}
