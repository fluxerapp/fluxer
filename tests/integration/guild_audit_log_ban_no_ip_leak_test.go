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
	"fmt"
	"net/http"
	"regexp"
	"strings"
	"testing"
	"time"
)

// TestGuildAuditLogBanNoIPLeak verifies that IP addresses are not exposed in guild ban audit logs
func TestGuildAuditLogBanNoIPLeak(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)
	target := createTestAccount(t, client)

	ensureSessionStarted(t, client, owner.Token)
	ensureSessionStarted(t, client, target.Token)

	guild := createGuild(t, client, owner.Token, fmt.Sprintf("Audit Log IP Test %d", time.Now().UnixNano()))
	guildID := parseSnowflake(t, guild.ID)
	channelID := parseSnowflake(t, guild.SystemChannel)

	invite := createChannelInvite(t, client, owner.Token, channelID)
	resp, err := client.postJSONWithAuth(fmt.Sprintf("/invites/%s", invite.Code), nil, target.Token)
	if err != nil {
		t.Fatalf("failed to accept invite: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.putJSONWithAuth(fmt.Sprintf("/guilds/%d/bans/%s", guildID, target.UserID), map[string]any{
		"delete_message_seconds": 0,
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to ban user: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/audit-logs", guildID), owner.Token)
	if err != nil {
		t.Fatalf("failed to get audit logs: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var auditLogResponse struct {
		AuditLogEntries []struct {
			ID         string `json:"id"`
			ActionType int    `json:"action_type"`
			UserID     string `json:"user_id"`
			TargetID   string `json:"target_id"`
			Reason     string `json:"reason"`
			Changes    []struct {
				Key      string      `json:"key"`
				OldValue interface{} `json:"old_value"`
				NewValue interface{} `json:"new_value"`
			} `json:"changes"`
		} `json:"audit_log_entries"`
	}
	decodeJSONResponse(t, resp, &auditLogResponse)

	if len(auditLogResponse.AuditLogEntries) == 0 {
		t.Fatalf("expected at least one audit log entry for the ban action")
	}

	ipPattern := regexp.MustCompile(`\b(?:\d{1,3}\.){3}\d{1,3}\b|(?:[0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}`)

	for _, entry := range auditLogResponse.AuditLogEntries {
		for _, change := range entry.Changes {
			if change.Key == "ip" {
				t.Errorf("audit log entry %s contains 'ip' key in changes - IP addresses must not be leaked", entry.ID)
			}

			if change.OldValue != nil {
				oldValStr := fmt.Sprintf("%v", change.OldValue)
				if ipPattern.MatchString(oldValStr) && !isSnowflakeID(oldValStr) {
					t.Errorf("audit log entry %s change key=%s old_value contains IP-like pattern: %v", entry.ID, change.Key, change.OldValue)
				}
			}
			if change.NewValue != nil {
				newValStr := fmt.Sprintf("%v", change.NewValue)
				if ipPattern.MatchString(newValStr) && !isSnowflakeID(newValStr) {
					t.Errorf("audit log entry %s change key=%s new_value contains IP-like pattern: %v", entry.ID, change.Key, change.NewValue)
				}
			}
		}
	}

	resp, err = client.getWithAuth(fmt.Sprintf("/guilds/%d/audit-logs", guildID), owner.Token)
	if err != nil {
		t.Fatalf("failed to get audit logs for raw check: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var rawResponse json.RawMessage
	decodeJSONResponse(t, resp, &rawResponse)
	rawStr := string(rawResponse)

	if strings.Contains(rawStr, `"ip"`) {
		t.Errorf("audit log raw response contains 'ip' key - IP addresses must not be leaked")
	}
}

// isSnowflakeID checks if a string looks like a snowflake ID (to avoid false positives with IDs containing periods)
func isSnowflakeID(s string) bool {
	for _, c := range s {
		if c < '0' || c > '9' {
			return false
		}
	}
	return len(s) >= 15 && len(s) <= 22
}
