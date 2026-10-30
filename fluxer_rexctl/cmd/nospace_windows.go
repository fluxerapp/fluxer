// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build windows

package cmd

import (
	"errors"
	"syscall"

	"golang.org/x/sys/windows"
)

func noSpace(err error) bool {
	return errors.Is(err, syscall.ENOSPC) || errors.Is(err, windows.ERROR_DISK_FULL) || errors.Is(err, windows.ERROR_HANDLE_DISK_FULL)
}
