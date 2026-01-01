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

type scheduledMessageResponse struct {
	ID               string  `json:"id"`
	ChannelID        string  `json:"channel_id"`
	Status           string  `json:"status"`
	StatusReason     *string `json:"status_reason"`
	ScheduledAt      string  `json:"scheduled_at"`
	ScheduledLocalAt string  `json:"scheduled_local_at"`
	Timezone         string  `json:"timezone"`
}

type channelMessage struct {
	Content string `json:"content"`
	Author  struct {
		ID string `json:"id"`
	} `json:"author"`
}

func TestScheduledMessageWorkerLifecycle(t *testing.T) {
	client := newTestClient(t)

	owner := registerTestUser(t, client, "sched-owner@example.com", "TestUncommonPw1!")
	guild := createGuild(t, client, owner.Token, "scheduled-messages")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "scheduled")
	channelID := parseSnowflake(t, channel.ID)

	t.Run("delivers scheduled message when permissions remain", func(t *testing.T) {
		content := "scheduled message goes through"
		scheduled := scheduleMessage(t, client, channelID, owner.Token, content)

		triggerScheduledMessageJob(t, client, owner.UserID, scheduled.ID)

		resp, err := client.getWithAuth(fmt.Sprintf("/users/@me/scheduled-messages/%s", scheduled.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to fetch scheduled message: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()

		messages := fetchChannelMessages(t, client, channelID, owner.Token)
		if !messageFromAuthorContains(messages, owner.UserID, content) {
			t.Fatalf("expected scheduled message %q from owner to appear in channel", content)
		}
	})

	t.Run("reschedules pending message before worker execution", func(t *testing.T) {
		content := "scheduled message initial content"
		scheduled := scheduleMessage(t, client, channelID, owner.Token, content)

		oldScheduledAt, err := time.Parse(time.RFC3339, scheduled.ScheduledAt)
		if err != nil {
			t.Fatalf("failed to parse original scheduled time: %v", err)
		}

		updatedContent := "scheduled message updated content"
		location, err := time.LoadLocation("America/Los_Angeles")
		if err != nil {
			t.Fatalf("failed to load timezone: %v", err)
		}
		newLocalTime := time.Now().In(location).Add(5 * time.Minute)
		newLocalStr := newLocalTime.Format(time.RFC3339)

		updated := updateScheduledMessage(t, client, scheduled.ID, owner.Token, map[string]string{
			"content":            updatedContent,
			"scheduled_local_at": newLocalStr,
			"timezone":           location.String(),
		})

		if updated.Status != "pending" {
			t.Fatalf("expected updated scheduled message to stay pending, got status=%q", updated.Status)
		}

		if updated.ScheduledLocalAt != newLocalStr {
			t.Fatalf("expected scheduled_local_at to update to %q, got %q", newLocalStr, updated.ScheduledLocalAt)
		}

		if updated.Timezone != location.String() {
			t.Fatalf("expected timezone to update to %q, got %q", location.String(), updated.Timezone)
		}

		updatedScheduledAt, err := time.Parse(time.RFC3339, updated.ScheduledAt)
		if err != nil {
			t.Fatalf("failed to parse updated scheduled time: %v", err)
		}

		if !updatedScheduledAt.After(oldScheduledAt) {
			t.Fatalf("expected updated scheduled time %s to be after previous time %s", updatedScheduledAt, oldScheduledAt)
		}

		triggerScheduledMessageJob(t, client, owner.UserID, updated.ID)

		respMessages := fetchChannelMessages(t, client, channelID, owner.Token)
		if !messageFromAuthorContains(respMessages, owner.UserID, updatedContent) {
			t.Fatalf("expected updated scheduled message %q to appear in channel", updatedContent)
		}

		if messageFromAuthorContains(respMessages, owner.UserID, content) {
			t.Fatalf("unexpected original scheduled content delivered after reschedule")
		}

		resp, err := client.getWithAuth(fmt.Sprintf("/users/@me/scheduled-messages/%s", updated.ID), owner.Token)
		if err != nil {
			t.Fatalf("failed to fetch scheduled message after reschedule execution: %v", err)
		}
		assertStatus(t, resp, http.StatusNotFound)
		resp.Body.Close()
	})

	t.Run("marks scheduled message invalid when access lost", func(t *testing.T) {
		member := registerTestUser(t, client, "sched-member@example.com", "TestUncommonPw1!")
		invite := createChannelInvite(t, client, owner.Token, channelID)
		joinGuild(t, client, member.Token, invite.Code)

		content := "scheduled message invalidation"
		scheduled := scheduleMessage(t, client, channelID, member.Token, content)

		resp, err := client.delete(fmt.Sprintf("/guilds/%d/members/%s", parseSnowflake(t, guild.ID), member.UserID), owner.Token)
		if err != nil {
			t.Fatalf("failed to remove member: %v", err)
		}
		assertStatus(t, resp, http.StatusNoContent)
		resp.Body.Close()

		triggerScheduledMessageJob(t, client, member.UserID, scheduled.ID)

		resp, err = client.getWithAuth(fmt.Sprintf("/users/@me/scheduled-messages/%s", scheduled.ID), member.Token)
		if err != nil {
			t.Fatalf("failed to fetch invalidated scheduled message: %v", err)
		}
		var fetched scheduledMessageResponse
		assertStatus(t, resp, http.StatusOK)
		decodeJSONResponse(t, resp, &fetched)
		if fetched.Status != "invalid" || fetched.StatusReason == nil || *fetched.StatusReason == "" {
			var reason string
			if fetched.StatusReason != nil {
				reason = *fetched.StatusReason
			}
			t.Fatalf("expected scheduled message invalidated, got status=%q reason=%q", fetched.Status, reason)
		}

		messages := fetchChannelMessages(t, client, channelID, owner.Token)
		if containsMessageContent(messages, content) {
			t.Fatalf("scheduled message should not appear in channel after invalidation")
		}
	})
}

func scheduleMessage(t testing.TB, client *testClient, channelID int64, token, content string) scheduledMessageResponse {
	t.Helper()

	payload := map[string]string{
		"content":            content,
		"scheduled_local_at": time.Now().UTC().Add(1 * time.Minute).Format(time.RFC3339),
		"timezone":           "UTC",
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/schedule", channelID), payload, token)
	if err != nil {
		t.Fatalf("failed to schedule message: %v", err)
	}
	assertStatus(t, resp, http.StatusCreated)

	var scheduled scheduledMessageResponse
	decodeJSONResponse(t, resp, &scheduled)
	resp.Body.Close()
	return scheduled
}

func updateScheduledMessage(t testing.TB, client *testClient, scheduledMessageID, token string, payload map[string]string) scheduledMessageResponse {
	t.Helper()

	resp, err := client.patchJSONWithAuth(fmt.Sprintf("/users/@me/scheduled-messages/%s", scheduledMessageID), payload, token)
	if err != nil {
		t.Fatalf("failed to update scheduled message: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var scheduled scheduledMessageResponse
	decodeJSONResponse(t, resp, &scheduled)
	resp.Body.Close()

	return scheduled
}

func triggerScheduledMessageJob(t testing.TB, client *testClient, userID, scheduledMessageID string) {
	t.Helper()

	resp, err := client.postJSON(fmt.Sprintf("/test/worker/send-scheduled-message/%s/%s", userID, scheduledMessageID), nil)
	if err != nil {
		t.Fatalf("failed to trigger scheduled message worker: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
}

func fetchChannelMessages(t testing.TB, client *testClient, channelID int64, token string) []channelMessage {
	t.Helper()

	resp, err := client.getWithAuth(fmt.Sprintf("/channels/%d/messages?limit=20", channelID), token)
	if err != nil {
		t.Fatalf("failed to fetch channel messages: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var messages []channelMessage
	decodeJSONResponse(t, resp, &messages)
	return messages
}

func messageFromAuthorContains(messages []channelMessage, authorID, content string) bool {
	for _, msg := range messages {
		if msg.Author.ID == authorID && msg.Content == content {
			return true
		}
	}
	return false
}

func containsMessageContent(messages []channelMessage, content string) bool {
	for _, msg := range messages {
		if msg.Content == content {
			return true
		}
	}
	return false
}
