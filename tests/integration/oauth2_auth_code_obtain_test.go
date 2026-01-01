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
	"testing"
)

// obtainAuthCode is a helper to obtain an authorization code for a given application/redirect using a fresh end-user.
func obtainAuthCode(t testing.TB, client *testClient, appID string, redirectURI string, scopes []string) (code string, user testAccount) {
	t.Helper()
	user = createTestAccount(t, client)
	code, _ = authorizeOAuth2(t, client, user.Token, appID, redirectURI, scopes, "", "", "")
	if code == "" {
		t.Fatalf("failed to obtain authorization code")
	}
	return code, user
}
