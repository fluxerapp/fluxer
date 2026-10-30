// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package cmd

import (
	"errors"
	"syscall"
)

func noSpace(err error) bool {
	return errors.Is(err, syscall.ENOSPC) || errors.Is(err, syscall.EDQUOT)
}
