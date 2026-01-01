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
	"time"
)

func (g *zstdGatewayClient) WaitForEvent(t testing.TB, eventType string, timeout time.Duration, matchFunc func(json.RawMessage) bool) json.RawMessage {
	t.Helper()

	g.pendingMu.Lock()
	for i, d := range g.pendingDispatch {
		if d.Type == eventType && (matchFunc == nil || matchFunc(d.Data)) {
			g.pendingDispatch = append(g.pendingDispatch[:i], g.pendingDispatch[i+1:]...)
			g.pendingMu.Unlock()
			return d.Data
		}
	}
	g.pendingMu.Unlock()

	deadline := time.After(timeout)
	for {
		select {
		case d := <-g.dispatchCh:
			if d.Type == eventType && (matchFunc == nil || matchFunc(d.Data)) {
				return d.Data
			}
			g.pendingMu.Lock()
			g.pendingDispatch = append(g.pendingDispatch, d)
			g.pendingMu.Unlock()
		case <-deadline:
			t.Fatalf("timeout waiting for event %s", eventType)
			return nil
		}
	}
}
