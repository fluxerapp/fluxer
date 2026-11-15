// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build windows

package release

import (
	"errors"
	"os"
	"os/exec"
	"os/signal"
)

var stdout = os.Stdout

func Reexec(args []string) error {
	if selfPathErr != nil {
		return selfPathErr
	}
	cmd := exec.Command(selfPath, args...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = stdout
	cmd.Stderr = os.Stderr
	signal.Ignore(os.Interrupt)
	err := cmd.Run()
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		os.Exit(exitErr.ExitCode())
	}
	if err != nil {
		return err
	}
	os.Exit(0)
	return nil
}
