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
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"testing"
)

func createTestAccount(t testing.TB, client *testClient, opts ...registerOption) testAccount {
	t.Helper()

	// Generate a short random hex string for unique email
	var randomBytes [6]byte
	if _, err := rand.Read(randomBytes[:]); err != nil {
		t.Fatalf("failed to generate random bytes: %v", err)
	}

	email := fmt.Sprintf("test-%s@example.com", hex.EncodeToString(randomBytes[:]))
	password := uniquePassword()
	resp := registerTestUser(t, client, email, password, opts...)

	updateUserSecurityFlags(t, client, resp.UserID, userSecurityFlagsPayload{
		SetFlags: []string{"HAS_SESSION_STARTED"},
	})

	return testAccount{
		UserID:   resp.UserID,
		Token:    resp.Token,
		Email:    email,
		Password: password,
	}
}
