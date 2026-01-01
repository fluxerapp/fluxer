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

// TestBotGatewayResumeSession verifies that bots can resume their gateway sessions
// after disconnection, maintaining their session state.
func TestBotGatewayResumeSession(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	appName := fmt.Sprintf("Resume Bot %d", time.Now().UnixNano())
	redirectURI := "https://example.com/callback"
	_, _, botToken := createOAuth2BotApplication(t, client, owner, appName, []string{redirectURI})

	gc := newGatewayClient(t, client, botToken)

	resumeState := gatewayResumeState{
		SessionID: gc.SessionID(),
		Sequence:  gc.Sequence(),
	}

	if resumeState.SessionID == "" {
		t.Fatalf("should have session ID after READY")
	}

	t.Logf("Initial session - ID: %s, Sequence: %d", resumeState.SessionID, resumeState.Sequence)

	gc.Close()

	time.Sleep(1 * time.Second)

	gcResume := newGatewayClientWithResume(t, client, botToken, resumeState)
	defer gcResume.Close()

	t.Logf("Session successfully resumed - SessionID: %s", resumeState.SessionID)
}
