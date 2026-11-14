// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"errors"
	"testing"
)

func TestDistFor(t *testing.T) {
	amd64 := Asset{OS: "linux", Arch: "amd64", URL: "https://example.org/amd64.tar.zst", SHA256: "aa"}
	arm64 := Asset{OS: "linux", Arch: "arm64", URL: "https://example.org/arm64.tar.zst", SHA256: "bb"}
	cases := []struct {
		name string
		dist []Asset
		arch string
		want Asset
		msg  string
	}{
		{"finds the arch", []Asset{amd64, arm64}, "arm64", arm64, ""},
		{"no dist at all", nil, "amd64", Asset{}, "Fluxer 2026.1112.120000 has no native build for linux/amd64 yet."},
		{"only the other arch", []Asset{amd64}, "arm64", Asset{}, "Fluxer 2026.1112.120000 has no native build for linux/arm64 yet."},
		{"ignores other systems", []Asset{{OS: "darwin", Arch: "arm64", URL: "x", SHA256: "cc"}}, "arm64", Asset{}, "Fluxer 2026.1112.120000 has no native build for linux/arm64 yet."},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			rel := &Release{Version: "2026.1112.120000", Dist: c.dist}
			got, err := rel.DistFor(c.arch)
			if c.msg == "" {
				if err != nil || got != c.want {
					t.Fatalf("got %+v, %v, want %+v", got, err, c.want)
				}
				return
			}
			var nb *NoNativeBuildError
			if !errors.Is(err, ErrNoNativeBuild) || !errors.As(err, &nb) || err.Error() != c.msg {
				t.Fatalf("got %v, want %q", err, c.msg)
			}
			if nb.Arch != c.arch || nb.Version != rel.Version {
				t.Fatalf("got %+v", nb)
			}
		})
	}
}
