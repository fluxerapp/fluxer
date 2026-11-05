// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import "testing"

func TestDerivedFollowsPublicScheme(t *testing.T) {
	cases := []struct {
		name   string
		chosen map[string]string
		origin string
		site   string
		port   string
	}{
		{"https default", map[string]string{Domain: "chat.example.com"}, "https://chat.example.com", "https://chat.example.com", "443"},
		{"https listen port", map[string]string{Domain: "chat.example.com", EdgeHTTPSPort: "8443"}, "https://chat.example.com:8443", "https://chat.example.com", "8443"},
		{"plain http", map[string]string{Domain: "chat.example.com", "FLUXER_PUBLIC_SCHEME": "http"}, "http://chat.example.com", "http://chat.example.com", "80"},
		{"plain http listen port", map[string]string{Domain: "chat.example.com", "FLUXER_PUBLIC_SCHEME": "http", EdgeHTTPPort: "8080", EdgeHTTPSPort: "8443"}, "http://chat.example.com:8080", "http://chat.example.com", "8080"},
	}
	for _, c := range cases {
		out := derived(c.chosen)
		if out["FLUXER_PUBLIC_ORIGIN"] != c.origin || out["FLUXER_EDGE_SITE_ADDRESS"] != c.site || out["FLUXER_PUBLIC_PORT"] != c.port {
			t.Fatalf("%s: got origin %q site %q port %q", c.name, out["FLUXER_PUBLIC_ORIGIN"], out["FLUXER_EDGE_SITE_ADDRESS"], out["FLUXER_PUBLIC_PORT"])
		}
	}
}
