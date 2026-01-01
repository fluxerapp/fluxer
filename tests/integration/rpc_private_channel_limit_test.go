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
	"sort"
	"testing"
	"time"
)

const (
	channelTypeDM      = 1
	channelTypeGroupDM = 3
	maxPrivateChannels = 250
)

func TestRpcSessionPrivateChannelLimit(t *testing.T) {
	client := newTestClient(t)
	user := createTestAccount(t, client)
	recipients := []testAccount{
		createTestAccount(t, client),
		createTestAccount(t, client),
		createTestAccount(t, client),
	}

	payload := map[string]any{
		"dm_count":       260,
		"group_dm_count": 10,
		"recipients": []string{
			recipients[0].UserID,
			recipients[1].UserID,
			recipients[2].UserID,
		},
		"clear_existing": true,
	}

	seedResult := seedPrivateChannels(t, client, user, payload)
	if len(seedResult.GroupDMs) != 10 {
		t.Fatalf("expected to seed 10 group DMs, got %d", len(seedResult.GroupDMs))
	}

	gateway := newGatewayClient(t, client, user.Token)
	defer gateway.Close()

	ready := gateway.WaitForEvent(t, "READY", 10*time.Second, nil)

	var readyPayload struct {
		PrivateChannels []rpcChannelResponse `json:"private_channels"`
	}
	if err := json.Unmarshal(ready.Data, &readyPayload); err != nil {
		t.Fatalf("failed to decode READY payload: %v", err)
	}

	privateChannels := readyPayload.PrivateChannels
	if len(privateChannels) != maxPrivateChannels {
		t.Fatalf("expected %d private channels, got %d", maxPrivateChannels, len(privateChannels))
	}

	dmMessageIDs := make([]int64, 0, maxPrivateChannels)
	groupCount := 0
	for _, channel := range privateChannels {
		switch channel.Type {
		case channelTypeGroupDM:
			groupCount++
		case channelTypeDM:
			if channel.LastMessageID == "" {
				t.Fatalf("private channel missing last_message_id")
			}
			dmMessageIDs = append(dmMessageIDs, parseSnowflake(t, channel.LastMessageID))
		}
	}

	if groupCount != len(seedResult.GroupDMs) {
		t.Fatalf("expected %d group DMs, got %d", len(seedResult.GroupDMs), groupCount)
	}

	expectedDMs := maxPrivateChannels - groupCount
	if len(dmMessageIDs) != expectedDMs {
		t.Fatalf("expected %d DM channels, got %d", expectedDMs, len(dmMessageIDs))
	}

	seedMessageIDs := make([]int64, len(seedResult.DMs))
	for i, entry := range seedResult.DMs {
		seedMessageIDs[i] = parseSnowflake(t, entry.LastMessageID)
	}

	sort.Slice(seedMessageIDs, func(i, j int) bool {
		return seedMessageIDs[i] < seedMessageIDs[j]
	})

	closedCount := len(seedMessageIDs) - expectedDMs
	if closedCount < 0 {
		t.Fatalf("closedCount (%d) unexpectedly negative", closedCount)
	}

	closedSet := make(map[int64]struct{}, closedCount)
	for i := 0; i < closedCount; i++ {
		closedSet[seedMessageIDs[i]] = struct{}{}
	}

	for _, messageID := range dmMessageIDs {
		if _, exists := closedSet[messageID]; exists {
			t.Fatalf("expected closed DM with last_message_id %d to be omitted", messageID)
		}
	}
}
