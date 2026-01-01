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

func (g *gatewayClient) WaitForEvent(t testing.TB, eventType string, timeout time.Duration, match func(json.RawMessage) bool) gatewayDispatch {
	t.Helper()
	deadline := time.Now().Add(timeout)
	var skipped []gatewayDispatch
	defer func() {
		for _, dispatch := range skipped {
			g.pushPending(dispatch)
		}
	}()

	for {
		dispatch, ok := g.popPending()
		if !ok {
			break
		}
		if dispatch.Type == eventType && (match == nil || match(dispatch.Data)) {
			for _, d := range skipped {
				g.pushPending(d)
			}
			skipped = nil
			return dispatch
		}
		skipped = append(skipped, dispatch)
	}

	for {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			t.Fatalf("timed out waiting for gateway event %s", eventType)
		}
		dispatch, err := g.nextDispatchFromChannel(remaining)
		if errors.Is(err, context.DeadlineExceeded) {
			t.Fatalf("timed out waiting for gateway event %s", eventType)
		}
		if err != nil {
			t.Fatalf("gateway client error while waiting for %s: %v", eventType, err)
		}
		if dispatch.Type == eventType && (match == nil || match(dispatch.Data)) {
			for _, d := range skipped {
				g.pushPending(d)
			}
			skipped = nil
			return dispatch
		}
		skipped = append(skipped, dispatch)
	}
}
