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
)

// verifyUserDataDeleted checks that all user data has been properly deleted or anonymized
func verifyUserDataDeleted(t testing.TB, client *testClient, userID string) {
	resp, err := client.get(fmt.Sprintf("/test/users/%s/data-exists", userID))
	if err != nil {
		t.Fatalf("failed to check user data: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Fatalf("expected status 200, got %d", resp.StatusCode)
	}

	var data userDataExistsResponse
	decodeJSONResponse(t, resp, &data)

	if !data.UserExists {
		t.Fatal("expected user to still exist (anonymized), but user was completely removed")
	}

	if !data.HasDeletedFlag {
		t.Error("expected user to have DELETED flag set")
	}

	if !data.EmailCleared {
		t.Error("expected email to be cleared")
	}

	if !data.PasswordCleared {
		t.Error("expected password to be cleared")
	}

	if data.RelationshipsCount != 0 {
		t.Errorf("expected 0 relationships, got %d", data.RelationshipsCount)
	}

	if data.SessionsCount != 0 {
		t.Errorf("expected 0 sessions, got %d", data.SessionsCount)
	}

	if data.OAuthTokensCount != 0 {
		t.Errorf("expected 0 oauth tokens, got %d", data.OAuthTokensCount)
	}

	if data.PinnedDmsCount != 0 {
		t.Errorf("expected 0 pinned DMs, got %d", data.PinnedDmsCount)
	}

	if data.SavedMessagesCount != 0 {
		t.Errorf("expected 0 saved messages, got %d", data.SavedMessagesCount)
	}

	t.Log("User data successfully deleted/anonymized")
}
