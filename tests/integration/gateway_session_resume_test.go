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

func TestGatewaySessionResume(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	initialSocket := newGatewayClient(t, client, account.Token)
	t.Cleanup(initialSocket.Close)

	sessionID := initialSocket.SessionID()
	if sessionID == "" {
		t.Fatalf("expected session id after READY")
	}
	sequence := initialSocket.Sequence()

	initialSocket.Close()

	resumedSocket := newGatewayClientWithResume(t, client, account.Token, gatewayResumeState{
		SessionID: sessionID,
		Sequence:  sequence,
	})
	t.Cleanup(resumedSocket.Close)

	resumedSocket.WaitForEvent(t, "RESUMED", 30*time.Second, nil)
}
