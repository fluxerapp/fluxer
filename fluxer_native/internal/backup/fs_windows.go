// SPDX-License-Identifier: AGPL-3.0-or-later

package backup

import (
	"errors"
	"os"
	"os/exec"
)

func freeBytes(string) (uint64, error) {
	return 0, errors.ErrUnsupported
}

func ownGroup(*exec.Cmd) {}

func killGroup(pid int) {
	if p, err := os.FindProcess(pid); err == nil {
		p.Kill()
	}
}
