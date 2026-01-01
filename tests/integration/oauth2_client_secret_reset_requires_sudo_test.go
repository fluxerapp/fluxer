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

// TestClientSecretResetRequiresSudo ensures rotation is rejected without sudo payload.
func TestClientSecretResetRequiresSudo(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appID, _, _, _ := createOAuth2Application(
		t, client, owner,
		fmt.Sprintf("Client Secret Sudo %d", time.Now().UnixNano()),
		[]string{"https://example.com/callback"},
		[]string{"identify"},
	)

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/oauth2/applications/%s/client-secret/reset", appID), map[string]any{}, owner.Token)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("rotation should require sudo payload")
	}
	if resp.StatusCode != http.StatusUnauthorized && resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 401/403 for missing sudo, got %d", resp.StatusCode)
	}
}
