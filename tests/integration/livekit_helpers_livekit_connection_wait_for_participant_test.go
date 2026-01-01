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
	"time"
)

// waitForParticipant waits for a participant with the given identity to join the room
func (lk *livekitConnection) waitForParticipant(identity string, timeout time.Duration) bool {
	lk.t.Helper()

	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		participants := lk.room.GetRemoteParticipants()
		for _, p := range participants {
			if p.Identity() == identity {
				lk.t.Logf("Found participant: %s", identity)
				return true
			}
		}
		time.Sleep(100 * time.Millisecond)
	}

	lk.t.Logf("Did not find participant '%s' after %v. Current participants:", identity, timeout)
	participants := lk.room.GetRemoteParticipants()
	for _, p := range participants {
		lk.t.Logf("  - %s", p.Identity())
	}

	return false
}
