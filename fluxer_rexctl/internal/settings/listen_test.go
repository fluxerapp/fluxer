// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"errors"
	"maps"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestSplitListenAddress(t *testing.T) {
	cases := []struct {
		name string
		in   map[string]string
		want map[string]string
	}{
		{"ipv4 both", map[string]string{EdgeHTTPPort: "192.168.24.35:80", EdgeHTTPSPort: "192.168.24.35:443"},
			map[string]string{EdgeHTTPPort: "80", EdgeHTTPSPort: "443", EdgeListen: "192.168.24.35"}},
		{"ipv6 both", map[string]string{EdgeHTTPPort: "[2001:db8::7]:8080", EdgeHTTPSPort: "[2001:db8::7]:8443"},
			map[string]string{EdgeHTTPPort: "8080", EdgeHTTPSPort: "8443", EdgeListen: "2001:db8::7"}},
		{"one plain", map[string]string{EdgeHTTPPort: "80", EdgeHTTPSPort: "192.168.24.35:443"},
			map[string]string{EdgeHTTPPort: "80", EdgeHTTPSPort: "443", EdgeListen: "192.168.24.35"}},
		{"plain ports", map[string]string{EdgeHTTPPort: "80"}, map[string]string{EdgeHTTPPort: "80"}},
	}
	for _, c := range cases {
		got := maps.Clone(c.in)
		if _, err := SplitListenAddress(got, nil); err != nil || !maps.Equal(got, c.want) {
			t.Fatalf("%s: got %v, err %v", c.name, got, err)
		}
	}

	var conflict *ListenConflict
	differ := map[string]string{EdgeHTTPPort: "192.168.24.35:80", EdgeHTTPSPort: "192.168.24.36:443"}
	if _, err := SplitListenAddress(differ, nil); !errors.As(err, &conflict) {
		t.Fatalf("differing addresses: err %v", err)
	}
	if want := "Run rexctl config set edge.http_port=80 to listen on 192.168.24.36 for both."; conflict.Fix() != want {
		t.Fatalf("fix %q", conflict.Fix())
	}
	if _, err := SplitListenAddress(differ, []string{EdgeHTTPPort}); err != nil || differ[EdgeListen] != "192.168.24.36" {
		t.Fatalf("repairing edge.http_port: got %v, err %v", differ, err)
	}
}

func TestOldReleaseKeepsListenAddressInPorts(t *testing.T) {
	set := &stack.Set{Vars: []string{"FLUXER_HTTP_PORT", "FLUXER_HTTPS_PORT"}}
	env, err := Effective(set, map[string]string{EdgeListen: "2001:db8::7", EdgeHTTPSPort: "8443"}, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	if env["FLUXER_HTTP_PORT"] != "[2001:db8::7]:80" || env["FLUXER_HTTPS_PORT"] != "[2001:db8::7]:8443" {
		t.Fatalf("got %q and %q", env["FLUXER_HTTP_PORT"], env["FLUXER_HTTPS_PORT"])
	}
}
