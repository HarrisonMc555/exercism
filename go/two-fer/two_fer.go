// Package twofer provides a reply that shares between another person.
package twofer

import (
	"fmt"
)

// ShareWith returns a reply that shares something with another person.
func ShareWith(name string) string {
	if name == "" {
		name = "you"
	}
	return fmt.Sprintf("One for %s, one for me.", name)
}
