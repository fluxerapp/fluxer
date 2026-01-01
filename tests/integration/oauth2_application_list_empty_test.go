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

// TestOAuth2ApplicationListEmpty validates that an empty list is returned when user has no applications.
func TestOAuth2ApplicationListEmpty(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	apps := listOAuth2Applications(t, client, owner.Token)

	if apps == nil {
		t.Fatalf("expected empty array, got nil")
	}
	if len(apps) != 0 {
		t.Fatalf("expected empty list for new user, got %d applications", len(apps))
	}
}
