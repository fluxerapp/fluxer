// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"os"

	"golang.org/x/sys/windows"
)

func PrepareConsole(files ...*os.File) {
	for _, f := range files {
		h := windows.Handle(f.Fd())
		var mode uint32
		if windows.GetConsoleMode(h, &mode) != nil {
			continue
		}
		_ = windows.SetConsoleMode(h, mode|windows.ENABLE_VIRTUAL_TERMINAL_PROCESSING)
	}
}
