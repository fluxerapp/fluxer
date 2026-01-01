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

func TestFavoriteMemeSendsAttachmentInPersonalNotes(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)

	ensureSessionStarted(t, client, user.Token)

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

	channelID := parseSnowflake(t, user.UserID)

	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/channels/%d/messages", channelID),
		map[string]any{"favorite_meme_id": meme.ID},
		user.Token,
	)
	if err != nil {
		t.Fatalf("failed to send favorite meme message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var created messageResponse
	decodeJSONResponse(t, resp, &created)

	messageID := parseSnowflake(t, created.ID)
	resp, err = client.getWithAuth(fmt.Sprintf("/channels/%d/messages/%d", channelID, messageID), user.Token)
	if err != nil {
		t.Fatalf("failed to fetch personal note message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var fetched struct {
		ID          string `json:"id"`
		Content     string `json:"content"`
		Attachments []struct {
			ID       string `json:"id"`
			Filename string `json:"filename"`
		} `json:"attachments"`
	}
	decodeJSONResponse(t, resp, &fetched)

	if fetched.ID != created.ID {
		t.Fatalf("expected message ID %s, got %s", created.ID, fetched.ID)
	}
	if len(fetched.Attachments) != 1 {
		t.Fatalf("expected one attachment, got %d", len(fetched.Attachments))
	}
	if fetched.Attachments[0].Filename != meme.Filename {
		t.Fatalf("expected attachment filename %q, got %q", meme.Filename, fetched.Attachments[0].Filename)
	}
}
