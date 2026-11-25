// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"os/exec"
)

func dpkgInterrupted(ctx context.Context) bool {
	if euid() != 0 {
		return false
	}
	if _, err := exec.LookPath("dpkg"); err != nil {
		return false
	}
	out, err := exec.CommandContext(ctx, "dpkg", "--audit").Output()
	return err == nil && len(bytes.TrimSpace(out)) > 0
}
