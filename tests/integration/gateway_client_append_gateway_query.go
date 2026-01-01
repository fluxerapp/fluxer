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
	"net/url"
	"os"
	"testing"
)

func appendGatewayQuery(t testing.TB, raw string) string {
	t.Helper()
	if raw == "" {
		t.Fatalf("gateway url is empty")
	}
	parsed, err := url.Parse(raw)
	if err != nil {
		t.Fatalf("invalid gateway url %q: %v", raw, err)
	}
	if !parsed.IsAbs() {
		base := os.Getenv("FLUXER_INTEGRATION_API_URL")
		if base == "" {
			t.Fatalf("relative gateway url %q but FLUXER_INTEGRATION_API_URL not set", raw)
		}
		baseURL, err := url.Parse(base)
		if err != nil {
			t.Fatalf("invalid FLUXER_INTEGRATION_API_URL %q: %v", base, err)
		}
		switch baseURL.Scheme {
		case "https":
			parsed.Scheme = "wss"
		default:
			parsed.Scheme = "ws"
		}
		parsed.Host = baseURL.Host
	}
	query := parsed.Query()
	query.Set("v", "1")
	query.Set("encoding", "json")
	parsed.RawQuery = query.Encode()
	if parsed.Path == "" {
		parsed.Path = "/"
	}
	return parsed.String()
}
