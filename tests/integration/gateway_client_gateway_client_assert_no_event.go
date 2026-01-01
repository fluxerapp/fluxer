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
	"context"
	"encoding/json"
	"errors"
	"testing"
	"time"
)

func (g *gatewayClient) AssertNoEvent(t testing.TB, eventType string, duration time.Duration, match func(json.RawMessage) bool) {
	t.Helper()
	deadline := time.Now().Add(duration)
	for {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			return
		}
		dispatch, err := g.nextDispatch(remaining)
		if errors.Is(err, context.DeadlineExceeded) {
			return
		}
		if err != nil {
			t.Fatalf("gateway client error while asserting no %s: %v", eventType, err)
		}
		if dispatch.Type == eventType {
			if match == nil || match(dispatch.Data) {
				t.Fatalf("received unexpected gateway event %s: %s", eventType, string(dispatch.Data))
			}
		}
		g.pushPending(dispatch)
	}
}
