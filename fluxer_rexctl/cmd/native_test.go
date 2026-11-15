// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"runtime"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func otherArch() string {
	if runtime.GOARCH == "arm64" {
		return "amd64"
	}
	return "arm64"
}

func fakeResolver(t *testing.T, releases map[string]release.Release, latest string) *[]string {
	t.Helper()
	var asked []string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		v := r.URL.Query().Get("version")
		asked = append(asked, v)
		rel, ok := releases[v]
		if v == "" {
			rel, ok = releases[latest]
		}
		if !ok {
			http.NotFound(w, r)
			return
		}
		_ = json.NewEncoder(w).Encode(rel)
	}))
	t.Cleanup(srv.Close)
	t.Setenv(release.ResolverURLEnv, srv.URL)
	return &asked
}

func TestNativeRelease(t *testing.T) {
	here := release.Asset{OS: "linux", Arch: runtime.GOARCH, URL: "https://example.org/here.tar.zst", SHA256: "aa"}
	there := release.Asset{OS: "linux", Arch: otherArch(), URL: "https://example.org/there.tar.zst", SHA256: "bb"}
	releases := map[string]release.Release{
		"2026.1110.80000":  {Version: "2026.1110.80000"},
		"2026.1111.90000":  {Version: "2026.1111.90000", Dist: []release.Asset{there}},
		"2026.1112.100000": {Version: "2026.1112.100000", Dist: []release.Asset{here, there}},
	}
	cases := []struct {
		name    string
		version string
		latest  string
		asked   string
		want    string
		refused string
	}{
		{"pinned release with a native build", "2026.1112.100000", "2026.1111.90000", "2026.1112.100000", "2026.1112.100000", ""},
		{"pinned release with no dist", "2026.1110.80000", "2026.1112.100000", "2026.1110.80000", "", "Fluxer 2026.1110.80000 has no native build for linux/" + runtime.GOARCH + " yet."},
		{"pinned release built for the other arch only", "2026.1111.90000", "2026.1112.100000", "2026.1111.90000", "", "Fluxer 2026.1111.90000 has no native build for linux/" + runtime.GOARCH + " yet."},
		{"a dev build asks for the latest release", "dev", "2026.1112.100000", "", "2026.1112.100000", ""},
		{"latest release with no native build", "", "2026.1110.80000", "", "", "Fluxer 2026.1110.80000 has no native build for linux/" + runtime.GOARCH + " yet."},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			asked := fakeResolver(t, releases, c.latest)
			rel, err := nativeRelease(context.Background(), c.version, "the fix")
			if len(*asked) != 1 || (*asked)[0] != c.asked {
				t.Fatalf("asked the resolver for %q, want %q", *asked, c.asked)
			}
			if c.refused == "" {
				if err != nil || rel.Version != c.want {
					t.Fatalf("got %+v, %v, want %s", rel, err, c.want)
				}
				return
			}
			var ee *ui.ExitError
			if !errors.As(err, &ee) || ee.Code != ui.ExitRefused || ee.Msg != c.refused || ee.Fix != "the fix" || !errors.Is(err, release.ErrNoNativeBuild) {
				t.Fatalf("got %+v, want a refusal saying %q", ee, c.refused)
			}
		})
	}
}

func TestNativeReleaseUnknownVersion(t *testing.T) {
	fakeResolver(t, nil, "")
	_, err := nativeRelease(context.Background(), "2026.1101.10000", "the fix")
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitRefused || errors.Is(err, release.ErrNoNativeBuild) {
		t.Fatalf("got %+v, want the unknown release refusal", ee)
	}
}

func TestNativeFailureNamesTheMissingBuild(t *testing.T) {
	rel := &release.Release{Version: "2026.1112.100000"}
	_, err := rel.DistFor(runtime.GOARCH)
	var ee *ui.ExitError
	if !errors.As(nativeFailure(err, "Couldn't download Fluxer."), &ee) {
		t.Fatalf("got %v", err)
	}
	want := "Fluxer 2026.1112.100000 has no native build for linux/" + runtime.GOARCH + " yet."
	if ee.Code != ui.ExitRefused || ee.Msg != want || ee.Fix == "" {
		t.Fatalf("got %+v, want %q", ee, want)
	}
}
