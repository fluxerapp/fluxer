// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build windows

package cmd

import "errors"

func execSudoProcess(args []string) error {
	return errors.New("sudo isn't available on Windows")
}
