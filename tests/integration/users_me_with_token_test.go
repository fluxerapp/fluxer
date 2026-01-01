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

func TestUsersMeWithToken(t *testing.T) {
	client := newTestClient(t)

	email := fmt.Sprintf("integration-usersme-%d@example.com", time.Now().UnixNano())
	password := uniquePassword()

	reg := registerTestUser(t, client, email, password)

	resp, err := client.getWithAuth("/users/@me", reg.Token)
	if err != nil {
		t.Fatalf("failed to call /users/@me: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("GET /users/@me returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var userResp userPrivateResponse
	decodeJSONResponse(t, resp, &userResp)

	if userResp.ID != reg.UserID {
		t.Fatalf("expected /users/@me id %s to match registered user %s", userResp.ID, reg.UserID)
	}
	if userResp.Email != email {
		t.Fatalf("expected /users/@me email %s to match registration email %s", userResp.Email, email)
	}
	if userResp.Username == "" {
		t.Fatalf("expected /users/@me to include username")
	}
}
