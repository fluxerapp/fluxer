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

const (
	maxGroupDmLimit     = 150
	maxGroupDmErrorCode = "MAX_GROUP_DMS"
)

func TestGroupDmRecipientLimit(t *testing.T) {
	client := newTestClient(t)
	creator := createTestAccount(t, client)
	target := createTestAccount(t, client)
	recipient := createTestAccount(t, client)
	helper := createTestAccount(t, client)

	createFriendship(t, client, creator, target)
	createFriendship(t, client, creator, recipient)

	payload := map[string]any{
		"group_dm_count": maxGroupDmLimit,
		"recipients":     []string{helper.UserID, recipient.UserID},
		"clear_existing": true,
	}
	seedResult := seedPrivateChannels(t, client, target, payload)
	if len(seedResult.GroupDMs) != maxGroupDmLimit {
		t.Fatalf("expected %d seeded group DMs, got %d", maxGroupDmLimit, len(seedResult.GroupDMs))
	}

	requestPayload := map[string]any{
		"recipients": []string{helper.UserID, target.UserID},
	}
	resp, err := client.postJSONWithAuth("/users/@me/channels", requestPayload, creator.Token)
	if err != nil {
		t.Fatalf("failed to add recipient: %v", err)
	}

	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("expected status %d when limit reached, got %d", http.StatusBadRequest, resp.StatusCode)
	}

	var errorBody struct {
		Code string `json:"code"`
	}
	decodeJSONResponse(t, resp, &errorBody)
	if errorBody.Code != maxGroupDmErrorCode {
		t.Fatalf("expected error code %s, got %s", maxGroupDmErrorCode, errorBody.Code)
	}
}
