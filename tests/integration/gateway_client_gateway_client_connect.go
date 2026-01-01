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
	"github.com/gorilla/websocket"
	"testing"
	"time"
)

func (g *gatewayClient) connect(t testing.TB, resume *gatewayResumeState) {
	t.Helper()

	fullURL := appendGatewayQuery(t, g.gatewayURL)
	dialer := websocket.Dialer{HandshakeTimeout: 15 * time.Second}
	conn, resp, err := dialer.Dial(fullURL, g.headers)
	if resp != nil {
		resp.Body.Close()
	}
	if err != nil {
		t.Fatalf("failed to dial gateway %s: %v", fullURL, err)
	}

	g.conn = conn
	g.wg.Add(1)
	go g.readLoop()

	hello := g.waitForHello(t, 10*time.Second)
	g.startHeartbeat(time.Duration(hello.HeartbeatInterval) * time.Millisecond)

	if resume != nil {
		if resume.SessionID == "" {
			t.Fatalf("cannot resume gateway session without session id")
		}
		g.sessionMu.Lock()
		g.sessionID = resume.SessionID
		g.sessionMu.Unlock()
		g.sequence.Store(resume.Sequence)
		g.sendResume(resume.SessionID, resume.Sequence)
		g.waitForResumed(t, 15*time.Second)
	} else {
		g.sendIdentify()
		g.waitForReady(t, 15*time.Second)
	}
}
