// SPDX-License-Identifier: AGPL-3.0-or-later

package mdx

import "strings"

func Prose(s string) string {
	var b strings.Builder
	code := false
	for _, r := range s {
		if r == '`' {
			code = !code
		}
		if !code && strings.ContainsRune("<{}", r) {
			b.WriteByte('\\')
		}
		b.WriteRune(r)
	}
	return b.String()
}
