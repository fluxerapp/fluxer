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

func clearTestEmails(t testing.TB, client *testClient) {
	t.Helper()
	resp, err := client.delete("/test/emails", "")
	if err != nil {
		t.Fatalf("failed to clear test emails: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("clearing test emails returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
}
