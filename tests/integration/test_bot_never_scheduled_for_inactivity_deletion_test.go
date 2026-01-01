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
	"testing"
	"time"
)

// TestBotNeverScheduledForInactivityDeletion verifies that bot accounts
// are never scheduled for inactivity-based deletion, regardless of their
// inactivity status.
func TestBotNeverScheduledForInactivityDeletion(t *testing.T) {
	client := newTestClient(t)

	bot := createTestAccount(t, client)

	setBotFlag(t, client, bot.UserID, true)

	threeYearsAgo := time.Now().AddDate(-3, 0, 0)
	setLastActiveAt(t, client, bot.UserID, threeYearsAgo)

	clearTestEmails(t, client)

	workerResp := triggerInactivityWorker(t, client)

	if workerResp.WarningsSent > 0 {
		t.Errorf("expected no warnings sent for bot, got %d", workerResp.WarningsSent)
	}

	if workerResp.DeletionsScheduled > 0 {
		t.Errorf("expected no deletions scheduled for bot, got %d", workerResp.DeletionsScheduled)
	}

	dataResp, err := client.get(fmt.Sprintf("/test/users/%s/data-exists", bot.UserID))
	if err != nil {
		t.Fatalf("failed to check bot data: %v", err)
	}
	defer dataResp.Body.Close()

	var data userDataExistsResponse
	decodeJSONResponse(t, dataResp, &data)

	if data.PendingDeletionAt != nil {
		t.Error("expected bot to not have pending_deletion_at, but it was set")
	}

	t.Log("Bot immunity test passed")
}
