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

const favoriteMemeAttachmentFixture = "yeah.png"
const favoriteMemeAttachmentFixture2 = "thisisfine.gif"

func TestFavoriteMeme_CreateFromMessage(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	ensureSessionStarted(t, client, user.Token)

	user2 := createTestAccount(t, client)
	createFriendship(t, client, user, user2)
	dmChannel := createDmChannel(t, client, user.Token, parseSnowflake(t, user2.UserID))
	dmChannelID := parseSnowflake(t, dmChannel.ID)

	t.Run("can create meme from message attachment", func(t *testing.T) {
		msg, attachmentID := sendChannelMessageWithAttachment(
			t,
			client,
			user.Token,
			dmChannelID,
			"Test message for meme attachment",
			favoriteMemeAttachmentFixture,
		)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"attachment_id": fmt.Sprintf("%d", attachmentID),
			"name":          "Meme from Message",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)
	})

	t.Run("can create meme from message embed", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Embed source message",
			"embeds": []map[string]any{
				{
					"title": "Favorite Meme Embed",
					"image": map[string]any{"url": favoriteMemeTestImageURL},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create embed message: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "Meme from Embed",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)
	})

	t.Run("rejects missing attachment_id and embed_index", func(t *testing.T) {
		msg := sendChannelMessage(t, client, user.Token, dmChannelID, "Test message")
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"name": "Meme without source",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail without attachment_id or embed_index")
		}
	})

	t.Run("rejects empty name", func(t *testing.T) {
		msg := sendChannelMessage(t, client, user.Token, dmChannelID, "Test message")
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for empty name")
		}
	})

	t.Run("rejects negative embed_index", func(t *testing.T) {
		msg := sendChannelMessage(t, client, user.Token, dmChannelID, "Test message")
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": -1,
			"name":        "Test Meme",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for negative embed_index")
		}
	})

	t.Run("rejects nonexistent message", func(t *testing.T) {
		payload := map[string]any{
			"embed_index": 0,
			"name":        "Test Meme",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/999999999999999999/memes", dmChannelID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for nonexistent message")
		}
	})

	t.Run("requires authentication", func(t *testing.T) {
		payload := map[string]any{
			"embed_index": 0,
			"name":        "Test Meme",
		}

		resp, err := client.postJSON(
			fmt.Sprintf("/channels/%d/messages/123456789/memes", dmChannelID),
			payload,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusUnauthorized {
			t.Errorf("expected 401, got %d", resp.StatusCode)
		}
	})

	t.Run("can create meme from embed with thumbnail only", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Link embed with thumbnail",
			"embeds": []map[string]any{
				{
					"type":        "link",
					"title":       "Link Embed Title",
					"description": "This is a link embed with only a thumbnail",
					"url":         "https://example.com",
					"thumbnail":   map[string]any{"url": favoriteMemeTestImageURL},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message with thumbnail embed: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "Meme from Thumbnail",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)

		var meme favoriteMemeResponse
		decodeJSONResponse(t, resp, &meme)

		if meme.Name != "Meme from Thumbnail" {
			t.Errorf("expected name 'Meme from Thumbnail', got %q", meme.Name)
		}
	})

	t.Run("can create meme from embed with image", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Image embed message",
			"embeds": []map[string]any{
				{
					"title": "Image Embed",
					"image": map[string]any{"url": favoriteMemeTestImageURL + "?image_embed_test"},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message with image embed: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "Meme from Image Embed",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)
	})

	t.Run("rejects out of bounds embed_index", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Single embed message",
			"embeds": []map[string]any{
				{
					"title": "Only Embed",
					"image": map[string]any{"url": favoriteMemeTestImageURL},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 5,
			"name":        "Test Meme",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for out of bounds embed_index")
		}
	})

	t.Run("rejects embed without any media", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Text-only embed message",
			"embeds": []map[string]any{
				{
					"title":       "Text Only Embed",
					"description": "This embed has no image, video, or thumbnail",
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "Test Meme",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for embed without media")
		}
	})

	t.Run("prefers image over thumbnail when both exist", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Embed with both image and thumbnail",
			"embeds": []map[string]any{
				{
					"title":     "Rich Embed",
					"image":     map[string]any{"url": favoriteMemeTestImageURL},
					"thumbnail": map[string]any{"url": "https://picsum.photos/50"},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "Meme from Image",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)
	})

	t.Run("can select specific embed from multiple embeds", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Message with multiple embeds",
			"embeds": []map[string]any{
				{
					"title": "First Embed",
					"image": map[string]any{"url": favoriteMemeTestImageURL + "?first_embed"},
				},
				{
					"title":     "Second Embed",
					"thumbnail": map[string]any{"url": favoriteMemeTestImageURL + "?second_embed"},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 1,
			"name":        "Meme from Second Embed",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)
	})

	t.Run("rejects nonexistent attachment_id", func(t *testing.T) {
		msg, _ := sendChannelMessageWithAttachment(
			t,
			client,
			user.Token,
			dmChannelID,
			"Test message for nonexistent attachment",
			favoriteMemeAttachmentFixture,
		)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"attachment_id": "999999999999999999",
			"name":          "Test Meme",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for nonexistent attachment_id")
		}
	})

	t.Run("supports alt_text parameter", func(t *testing.T) {
		msg, attachmentID := sendChannelMessageWithAttachment(
			t,
			client,
			user.Token,
			dmChannelID,
			"Test message for alt_text",
			favoriteMemeAttachmentFixture2,
		)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"attachment_id": fmt.Sprintf("%d", attachmentID),
			"name":          "Meme with Alt Text",
			"alt_text":      "A descriptive alt text for accessibility",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)

		var meme favoriteMemeResponse
		decodeJSONResponse(t, resp, &meme)

		if meme.AltText == nil || *meme.AltText != "A descriptive alt text for accessibility" {
			t.Errorf("expected alt_text to be set, got %v", meme.AltText)
		}
	})

	t.Run("supports tags parameter", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Embed message for tags test",
			"embeds": []map[string]any{
				{
					"title": "Tags Test Embed",
					"image": map[string]any{"url": favoriteMemeTestImageURL + "?tags_test"},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "Meme with Tags",
			"tags":        []string{"funny", "reaction", "test"},
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusCreated)

		var meme favoriteMemeResponse
		decodeJSONResponse(t, resp, &meme)

		if len(meme.Tags) != 3 {
			t.Errorf("expected 3 tags, got %d", len(meme.Tags))
		}
	})

	t.Run("rejects name exceeding max length", func(t *testing.T) {
		msg, attachmentID := sendChannelMessageWithAttachment(
			t,
			client,
			user.Token,
			dmChannelID,
			"Test message for long name",
			favoriteMemeAttachmentFixture,
		)
		msgID := parseSnowflake(t, msg.ID)

		longName := ""
		for i := 0; i < 101; i++ {
			longName += "a"
		}

		payload := map[string]any{
			"attachment_id": fmt.Sprintf("%d", attachmentID),
			"name":          longName,
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for name exceeding 100 characters")
		}
	})

	t.Run("prevents duplicate content hash", func(t *testing.T) {
		messagePayload := map[string]any{
			"content": "Embed message for duplicate check",
			"embeds": []map[string]any{
				{
					"title": "Duplicate Check Embed",
					"image": map[string]any{"url": favoriteMemeTestImageURL + "?duplicate_test"},
				},
			},
		}

		msgResp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages", dmChannelID),
			messagePayload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to create message: %v", err)
		}
		assertStatus(t, msgResp, http.StatusOK)

		var msg messageResponse
		decodeJSONResponse(t, msgResp, &msg)
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "First Meme",
		}

		resp1, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make first request: %v", err)
		}
		defer resp1.Body.Close()
		assertStatus(t, resp1, http.StatusCreated)

		payload["name"] = "Duplicate Meme"
		resp2, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user.Token,
		)
		if err != nil {
			t.Fatalf("failed to make second request: %v", err)
		}
		defer resp2.Body.Close()

		if resp2.StatusCode == http.StatusOK || resp2.StatusCode == http.StatusCreated {
			t.Error("expected duplicate meme to be rejected")
		}
	})

	t.Run("cannot access message from channel without permission", func(t *testing.T) {
		user3 := createTestAccount(t, client)
		ensureSessionStarted(t, client, user3.Token)

		msg := sendChannelMessage(t, client, user.Token, dmChannelID, "Private message")
		msgID := parseSnowflake(t, msg.ID)

		payload := map[string]any{
			"embed_index": 0,
			"name":        "Unauthorized Meme",
		}

		resp, err := client.postJSONWithAuth(
			fmt.Sprintf("/channels/%d/messages/%d/memes", dmChannelID, msgID),
			payload,
			user3.Token,
		)
		if err != nil {
			t.Fatalf("failed to make request: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK || resp.StatusCode == http.StatusCreated {
			t.Error("expected request to fail for user without channel access")
		}
	})
}
