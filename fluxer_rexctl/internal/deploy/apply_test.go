// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"slices"
	"testing"
)

func TestMountRestarts(t *testing.T) {
	saved := map[string]string{"ca": "c", "certs": "a", "caddy.d": "b"}
	cases := []struct {
		name    string
		files   []string
		mounted map[string]string
		want    []string
	}{
		{"nothing changed", nil, map[string]string{"ca": "c", "certs": "a", "caddy.d": "b"}, nil},
		{"ca changed", nil, map[string]string{"ca": "x", "certs": "a", "caddy.d": "b"}, []string{"api", "worker"}},
		{"certs changed", nil, map[string]string{"ca": "c", "certs": "x", "caddy.d": "b"}, []string{"edge"}},
		{"caddy.d changed", nil, map[string]string{"ca": "c", "certs": "a", "caddy.d": "x"}, []string{"edge"}},
		{"Caddyfile and ca changed", []string{"edge"}, map[string]string{"ca": "x", "certs": "a", "caddy.d": "b"}, []string{"api", "edge", "worker"}},
	}
	for _, c := range cases {
		if got := mountRestarts(c.files, c.mounted, saved); !slices.Equal(got, c.want) {
			t.Errorf("%s: mountRestarts = %v, want %v", c.name, got, c.want)
		}
	}
}
