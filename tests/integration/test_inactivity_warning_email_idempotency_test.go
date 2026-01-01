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
	"testing"
	"time"
)

// TestInactivityWarningEmailIdempotency verifies that the inactivity warning
// email is only sent once to a user, and subsequent runs of the worker do not
// send duplicate warnings within the 30-day grace period.
func TestInactivityWarningEmailIdempotency(t *testing.T) {
	client := newTestClient(t)

	user := createTestAccount(t, client)

	threeYearsAgo := time.Now().AddDate(-3, 0, 0)
	setLastActiveAt(t, client, user.UserID, threeYearsAgo)

	clearTestEmails(t, client)

	t.Log("Running inactivity worker - first time")
	workerResp1 := triggerInactivityWorker(t, client)

	if workerResp1.WarningsSent != 1 {
		t.Errorf("expected 1 warning sent on first run, got %d", workerResp1.WarningsSent)
	}

	if workerResp1.DeletionsScheduled != 0 {
		t.Errorf("expected 0 deletions scheduled on first run, got %d", workerResp1.DeletionsScheduled)
	}

	emails1 := getTestEmails(t, client)
	if len(emails1) != 1 {
		t.Errorf("expected 1 email sent after first run, got %d", len(emails1))
	}
	const expectedWarningSubject = "Your Fluxer account will be deleted due to inactivity"
	if len(emails1) > 0 && emails1[0].Subject != expectedWarningSubject {
		t.Errorf("expected inactivity warning email subject %q, got %q", expectedWarningSubject, emails1[0].Subject)
	}

	clearTestEmails(t, client)

	t.Log("Running inactivity worker - second time")
	workerResp2 := triggerInactivityWorker(t, client)

	if workerResp2.WarningsSent > 0 {
		t.Errorf("expected 0 warnings sent on second run (idempotency), got %d", workerResp2.WarningsSent)
	}

	if workerResp2.DeletionsScheduled > 0 {
		t.Errorf("expected 0 deletions scheduled on second run, got %d", workerResp2.DeletionsScheduled)
	}

	emails2 := getTestEmails(t, client)
	if len(emails2) > 0 {
		t.Errorf("expected no emails sent on second run (idempotency), got %d", len(emails2))
	}

	t.Log("Inactivity warning email idempotency test passed")
}
