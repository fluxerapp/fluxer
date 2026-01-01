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

func TestScheduledMessageValidationRejectsPastTime(t *testing.T) {
	client := newTestClient(t)

	owner := registerTestUser(t, client, "sched-past-owner@example.com", "TestUncommonPw1!")
	guild := createGuild(t, client, owner.Token, "sched-validation-past")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "test")
	channelID := parseSnowflake(t, channel.ID)

	pastTime := time.Now().UTC().Add(-1 * time.Hour).Format(time.RFC3339)

	payload := map[string]string{
		"content":            "should fail - past time",
		"scheduled_local_at": pastTime,
		"timezone":           "UTC",
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/schedule", channelID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to attempt scheduling past message: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusBadRequest)

	var errorResp struct {
		Errors map[string]struct {
			Code    string `json:"_errors"`
			Message string `json:"message"`
		} `json:"errors"`
	}
	decodeJSONResponse(t, resp, &errorResp)

	if _, hasScheduledLocalAt := errorResp.Errors["scheduled_local_at"]; !hasScheduledLocalAt {
		t.Fatalf("expected validation error on scheduled_local_at field for past time")
	}
}

func TestScheduledMessageValidationRejectsExceeds30Days(t *testing.T) {
	client := newTestClient(t)

	owner := registerTestUser(t, client, "sched-30day-owner@example.com", "TestUncommonPw1!")
	guild := createGuild(t, client, owner.Token, "sched-validation-30day")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "test")
	channelID := parseSnowflake(t, channel.ID)

	futureTime := time.Now().UTC().Add(31 * 24 * time.Hour).Format(time.RFC3339)

	payload := map[string]string{
		"content":            "should fail - exceeds 30 days",
		"scheduled_local_at": futureTime,
		"timezone":           "UTC",
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/schedule", channelID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to attempt scheduling message >30 days: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusBadRequest)

	var errorResp struct {
		Errors map[string]struct {
			Code    string `json:"_errors"`
			Message string `json:"message"`
		} `json:"errors"`
	}
	decodeJSONResponse(t, resp, &errorResp)

	if _, hasScheduledLocalAt := errorResp.Errors["scheduled_local_at"]; !hasScheduledLocalAt {
		t.Fatalf("expected validation error on scheduled_local_at field for >30 days")
	}
}

func TestScheduledMessageValidationRejectsInvalidTimezone(t *testing.T) {
	client := newTestClient(t)

	owner := registerTestUser(t, client, "sched-tz-owner@example.com", "TestUncommonPw1!")
	guild := createGuild(t, client, owner.Token, "sched-validation-tz")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "test")
	channelID := parseSnowflake(t, channel.ID)

	futureTime := time.Now().UTC().Add(5 * time.Minute).Format(time.RFC3339)

	payload := map[string]string{
		"content":            "should fail - invalid timezone",
		"scheduled_local_at": futureTime,
		"timezone":           "Invalid/NotATimezone",
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/schedule", channelID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to attempt scheduling message with invalid timezone: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusBadRequest)

	var errorResp struct {
		Errors map[string]struct {
			Code    string `json:"_errors"`
			Message string `json:"message"`
		} `json:"errors"`
	}
	decodeJSONResponse(t, resp, &errorResp)

	if _, hasTimezone := errorResp.Errors["timezone"]; !hasTimezone {
		t.Fatalf("expected validation error on timezone field for invalid timezone")
	}
}

func TestScheduledMessageValidationAcceptsBoundary30Days(t *testing.T) {
	client := newTestClient(t)

	owner := registerTestUser(t, client, "sched-boundary-owner@example.com", "TestUncommonPw1!")
	guild := createGuild(t, client, owner.Token, "sched-validation-boundary")
	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "test")
	channelID := parseSnowflake(t, channel.ID)

	futureTime := time.Now().UTC().Add(29*24*time.Hour + 23*time.Hour).Format(time.RFC3339)

	payload := map[string]string{
		"content":            "should succeed - within 30 days",
		"scheduled_local_at": futureTime,
		"timezone":           "UTC",
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/channels/%d/messages/schedule", channelID), payload, owner.Token)
	if err != nil {
		t.Fatalf("failed to schedule message at boundary: %v", err)
	}
	defer resp.Body.Close()

	assertStatus(t, resp, http.StatusCreated)

	var scheduled scheduledMessageResponse
	decodeJSONResponse(t, resp, &scheduled)

	if scheduled.Status != "pending" {
		t.Fatalf("expected scheduled message to be pending, got status=%q", scheduled.Status)
	}
}
