// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package ui

import "os"

func PrepareConsole(...*os.File) {}
