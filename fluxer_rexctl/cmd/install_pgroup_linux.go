// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"os/exec"
	"syscall"
)

func ownProcessGroup(c *exec.Cmd) {
	c.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}
