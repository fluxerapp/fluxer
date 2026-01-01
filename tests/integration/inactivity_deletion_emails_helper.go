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
	"encoding/json"
	"net/http"
	"testing"
)

// getTestEmails retrieves all test emails that have been sent
func getTestEmails(t testing.TB, client *testClient) []testEmail {
	t.Helper()

	resp, err := client.get("/test/emails")
	if err != nil {
		t.Fatalf("failed to get test emails: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected status 200 when getting emails, got %d", resp.StatusCode)
	}

	var result testEmailListResponse
	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		t.Fatalf("failed to decode email response: %v", err)
	}

	return result.Emails
}
