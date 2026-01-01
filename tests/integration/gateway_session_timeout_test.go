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
	"strings"
	"testing"
	"time"

	"github.com/gorilla/websocket"
)

func TestGatewaySessionTimeout(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	gc := newGatewayClient(t, client, account.Token)
	sessionID := gc.SessionID()
	sequence := gc.Sequence()

	gc.WaitForEvent(t, "READY", 10*time.Second, nil)

	gc.Close()

	time.Sleep(15 * time.Second)

	resume := gatewayResumeState{
		SessionID: sessionID,
		Sequence:  sequence,
	}

	gcResumed := &gatewayClient{
		token:         account.Token,
		gatewayURL:    buildGatewayURL(t),
		headers:       gc.headers,
		helloCh:       make(chan gatewayHelloPayload, 1),
		readySignal:   make(chan struct{}, 1),
		resumedSignal: make(chan struct{}, 1),
		dispatchCh:    make(chan gatewayDispatch, 512),
		errCh:         make(chan error, 2),
	}
	defer gcResumed.Close()

	fullURL := appendGatewayQuery(t, gcResumed.gatewayURL)
	dialer := websocket.Dialer{HandshakeTimeout: 15 * time.Second}
	conn, _, err := dialer.Dial(fullURL, gcResumed.headers)
	if err != nil {
		t.Fatalf("failed to dial gateway for resume: %v", err)
	}

	gcResumed.conn = conn
	gcResumed.wg.Add(1)
	go gcResumed.readLoop()

	hello := gcResumed.waitForHello(t, 10*time.Second)
	gcResumed.startHeartbeat(time.Duration(hello.HeartbeatInterval) * time.Millisecond)

	gcResumed.sessionMu.Lock()
	gcResumed.sessionID = resume.SessionID
	gcResumed.sessionMu.Unlock()
	gcResumed.sequence.Store(resume.Sequence)
	gcResumed.sendResume(resume.SessionID, resume.Sequence)

	select {
	case err := <-gcResumed.errCh:
		if strings.Contains(err.Error(), "gateway session invalid") {
			return
		}
		t.Fatalf("unexpected error during resume: %v", err)
	case <-gcResumed.resumedSignal:
		t.Fatalf("session resumed unexpectedly (should have timed out)")
	case <-time.After(15 * time.Second):
		t.Fatalf("timed out waiting for invalid session error")
	}
}
