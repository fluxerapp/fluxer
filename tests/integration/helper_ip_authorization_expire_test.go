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
	"net/http"
	"testing"
)

// expireIPAuthorization invalidates a ticket and/or token immediately using the
// test harness, letting expiration-sensitive tests run without long sleeps.
func expireIPAuthorization(t testing.TB, client *testClient, ticket, token string) {
	t.Helper()

	payload := map[string]any{}
	if ticket != "" {
		payload["ticket"] = ticket
	}
	if token != "" {
		payload["token"] = token
	}
	if len(payload) == 0 {
		t.Fatalf("expireIPAuthorization requires a ticket or token")
	}

	resp, err := client.postJSON("/test/auth/ip-authorization/expire", payload)
	if err != nil {
		t.Fatalf("failed to expire ip authorization: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expire ip authorization returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
}
