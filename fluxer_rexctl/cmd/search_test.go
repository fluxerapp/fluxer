// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import "testing"

func TestOlderSearch(t *testing.T) {
	for _, c := range []struct {
		to, from string
		want     bool
	}{
		{"getmeili/meilisearch:v1.12", "getmeili/meilisearch:v1.53", true},
		{"getmeili/meilisearch:v1.53", "getmeili/meilisearch:v1.12", false},
		{"getmeili/meilisearch:v1.53.3", "docker.io/getmeili/meilisearch:v1.53@sha256:abc", false},
		{"registry:5000/meilisearch", "getmeili/meilisearch:v1.53", true},
	} {
		if got := olderSearch(c.to, c.from); got != c.want {
			t.Errorf("olderSearch(%q, %q) = %v, want %v", c.to, c.from, got, c.want)
		}
	}
}
