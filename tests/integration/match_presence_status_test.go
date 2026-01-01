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
	"encoding/json"
	"testing"
)

func TestMatchPresenceStatus(t *testing.T) {
	t.Run("matches correct user and status", func(t *testing.T) {
		expectedUserID := "123456789"
		expectedStatus := "online"

		testData := map[string]any{
			"user": map[string]any{
				"id": expectedUserID,
			},
			"status": expectedStatus,
		}

		rawData, _ := json.Marshal(testData)
		matcher := matchPresenceStatus(expectedUserID, expectedStatus)

		if !matcher(rawData) {
			t.Error("matcher should return true for matching user ID and status")
		}
	})

	t.Run("rejects wrong user ID", func(t *testing.T) {
		expectedUserID := "123456789"
		expectedStatus := "online"

		testData := map[string]any{
			"user": map[string]any{
				"id": "987654321",
			},
			"status": expectedStatus,
		}

		rawData, _ := json.Marshal(testData)
		matcher := matchPresenceStatus(expectedUserID, expectedStatus)

		if matcher(rawData) {
			t.Error("matcher should return false for wrong user ID")
		}
	})

	t.Run("rejects wrong status", func(t *testing.T) {
		expectedUserID := "123456789"
		expectedStatus := "online"

		testData := map[string]any{
			"user": map[string]any{
				"id": expectedUserID,
			},
			"status": "offline",
		}

		rawData, _ := json.Marshal(testData)
		matcher := matchPresenceStatus(expectedUserID, expectedStatus)

		if matcher(rawData) {
			t.Error("matcher should return false for wrong status")
		}
	})

	t.Run("rejects guild presence updates", func(t *testing.T) {
		expectedUserID := "123456789"
		expectedStatus := "online"

		testData := map[string]any{
			"user": map[string]any{
				"id": expectedUserID,
			},
			"status":   expectedStatus,
			"guild_id": "987654321",
		}

		rawData, _ := json.Marshal(testData)
		matcher := matchPresenceStatus(expectedUserID, expectedStatus)

		if matcher(rawData) {
			t.Error("matcher should return false for guild presence updates")
		}

		anyScopeMatcher := matchPresenceStatusAnyScope(expectedUserID, expectedStatus)
		if !anyScopeMatcher(rawData) {
			t.Error("any-scope matcher should return true for guild presence updates")
		}
	})
}
