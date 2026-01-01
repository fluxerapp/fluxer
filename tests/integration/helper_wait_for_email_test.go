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
	"time"
)

func waitForEmail(t testing.TB, client *testClient, emailType, to string) testEmail {
	t.Helper()
	timeout := time.After(30 * time.Second)
	ticker := time.NewTicker(200 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case <-timeout:
			t.Fatalf("timed out waiting for email type %s to %s", emailType, to)
		case <-ticker.C:
			resp, err := client.get("/test/emails")
			if err != nil {
				continue
			}
			if resp.StatusCode != http.StatusOK {
				resp.Body.Close()
				continue
			}
			var payload testEmailListResponse
			decodeJSONResponse(t, resp, &payload)
			for _, email := range payload.Emails {
				if email.Type == emailType && (to == "" || email.To == to) {
					return email
				}
			}
		}
	}
}
