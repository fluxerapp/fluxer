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
)

// matchPresenceStatus returns a function that matches presence updates for a specific user
// with a specific status, but only for global presence (not guild-specific).
func matchPresenceStatus(expectedUserID, expectedStatus string) func(json.RawMessage) bool {
	return func(raw json.RawMessage) bool {
		payload, ok := parsePresencePayload(raw, expectedUserID, expectedStatus)
		if !ok {
			return false
		}
		if _, hasGuild := payload["guild_id"]; hasGuild {
			return false
		}
		return true
	}
}

// matchPresenceStatusAnyScope matches a presence update regardless of whether it's global
// or tied to a guild.
func matchPresenceStatusAnyScope(expectedUserID, expectedStatus string) func(json.RawMessage) bool {
	return func(raw json.RawMessage) bool {
		_, ok := parsePresencePayload(raw, expectedUserID, expectedStatus)
		return ok
	}
}

func parsePresencePayload(raw json.RawMessage, expectedUserID, expectedStatus string) (map[string]any, bool) {
	var payload map[string]any
	if err := json.Unmarshal(raw, &payload); err != nil {
		return nil, false
	}
	user, ok := payload["user"].(map[string]any)
	if !ok {
		return nil, false
	}
	id, ok := user["id"].(string)
	if !ok || id != expectedUserID {
		return nil, false
	}
	status, ok := payload["status"].(string)
	if !ok || status != expectedStatus {
		return nil, false
	}
	return payload, true
}
