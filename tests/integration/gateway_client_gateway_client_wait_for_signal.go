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

func (g *gatewayClient) waitForSignal(t testing.TB, ch <-chan struct{}, timeout time.Duration, label string) {
	t.Helper()
	deadline := time.Now().Add(timeout)
	for {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			t.Fatalf("timed out waiting for gateway %s event", label)
		}

		select {
		case <-ch:
			return
		case err := <-g.errCh:
			t.Fatalf("gateway error while waiting for %s: %v", label, err)
		case <-time.After(remaining):
			t.Fatalf("timed out waiting for gateway %s event", label)
		}
	}
}
