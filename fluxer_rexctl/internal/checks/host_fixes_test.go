// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import "testing"

func TestHostChangingFixesAsk(t *testing.T) {
	for _, id := range []string{"podman-setup", "engine", "memory-overcommit", "low-ports", "boot-unit"} {
		c, ok := Get(id)
		if !ok {
			t.Fatalf("check %s is not registered", id)
		}
		if c.SafeFix {
			t.Errorf("%s changes this machine outside the instance folder, so doctor --fix must ask first", id)
		}
	}
}
