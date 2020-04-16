/*
Reply in a way that shares between another person.
*/
package twofer

import (
	"fmt"
)

func ShareWith(name string) string {
	if name == "" {
		name = "you"
	}
	return fmt.Sprintf("One for %s, one for me.", name)
}
