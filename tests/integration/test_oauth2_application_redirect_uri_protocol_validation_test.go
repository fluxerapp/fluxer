/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package integration

import (
	"fmt"
	"net/http"
	"testing"
	"time"
)

// TestOAuth2ApplicationRedirectURIProtocolValidation ensures redirect URI validation enforces safe schemes.
func TestOAuth2ApplicationRedirectURIProtocolValidation(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	allowed := []string{
		"https://example.com/callback",
		"http://localhost:8080/callback",
		"http://127.0.0.1/callback",
		"http://[::1]/callback",
		"http://foo.localhost/callback",
	}

	for _, redirect := range allowed {
		t.Run(fmt.Sprintf("accepts %s", redirect), func(t *testing.T) {
			resp, err := client.postJSONWithAuth("/oauth2/applications", map[string]any{
				"name":          fmt.Sprintf("Allowed Redirect %d", time.Now().UnixNano()),
				"redirect_uris": []string{redirect},
			}, owner.Token)
			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusOK {
				t.Fatalf("expected status %d, got %d: %s", http.StatusOK, resp.StatusCode, readResponseBody(resp))
			}
		})
	}

	disallowed := []struct {
		name string
		uri  string
	}{
		{name: "javascript", uri: "javascript://example.com/%0Aalert(document.cookie)"},
		{name: "data", uri: "data://example.com/text"},
		{name: "file", uri: "file://example.com/etc/passwd"},
		{name: "vbscript", uri: "vbscript://example.com/code"},
		{name: "ftp", uri: "ftp://example.com/file"},
		{name: "ws", uri: "ws://example.com/socket"},
		{name: "wss", uri: "wss://example.com/socket"},
		{name: "custom", uri: "custom://example.com/path"},
	}

	for _, tt := range disallowed {
		t.Run(fmt.Sprintf("rejects %s", tt.name), func(t *testing.T) {
			resp, err := client.postJSONWithAuth("/oauth2/applications", map[string]any{
				"name":          fmt.Sprintf("Disallowed Redirect %s", tt.name),
				"redirect_uris": []string{tt.uri},
			}, owner.Token)
			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusBadRequest {
				t.Fatalf("expected status %d, got %d: %s", http.StatusBadRequest, resp.StatusCode, readResponseBody(resp))
			}
		})
	}
}
