// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package cmd

import (
	"os"
	"os/exec"
	"syscall"
)

func execSudoProcess(args []string) error {
	sudo, err := exec.LookPath("sudo")
	if err != nil {
		return err
	}
	return syscall.Exec(sudo, append([]string{"sudo"}, args...), os.Environ())
}
