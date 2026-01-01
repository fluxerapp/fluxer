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

func TestFavoriteMemeAttachmentsNeverHitDecay(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	channelID := parseSnowflake(t, user.UserID)

	resp, err := client.postJSONWithAuth(
		"/users/@me/memes",
		map[string]string{"url": favoriteMemeTestImageURL},
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to create favorite meme: %v", err)
	}
	assertStatus(t, resp, http.StatusCreated)

	var meme favoriteMemeResponse
	decodeJSONResponse(t, resp, &meme)

	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", channelID),
		map[string]any{"favorite_meme_id": meme.ID},
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to send favorite meme message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var created struct {
		ID          string `json:"id"`
		Attachments []struct {
			ID string `json:"id"`
		} `json:"attachments"`
	}
	decodeJSONResponse(t, resp, &created)

	if len(created.Attachments) == 0 {
		t.Fatalf("expected favorite meme message to include attachments")
	}

	attachmentID := created.Attachments[0].ID

	assertAttachmentDecayRowMissing(t, client, attachmentID, user.Token)

	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%s", channelID, created.ID), user.Token)
	if err != nil {
		t.Fatalf("failed to fetch favorite meme message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	assertAttachmentDecayRowMissing(t, client, attachmentID, user.Token)
}

func assertAttachmentDecayRowMissing(t testing.TB, client *testClient, attachmentID, token string) {
	t.Helper()

	resp, err := client.getWithAuth(fmt.Sprintf("/test/attachment-decay/%s", attachmentID), token)
	if err != nil {
		t.Fatalf("failed to query attachment decay row: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var payload struct {
		Row *struct {
			AttachmentID string `json:"attachment_id"`
		} `json:"row"`
	}
	decodeJSONResponse(t, resp, &payload)
	if payload.Row != nil {
		t.Fatalf("expected no attachment decay entry for %s", attachmentID)
	}
}
