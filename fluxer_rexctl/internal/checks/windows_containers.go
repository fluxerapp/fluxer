// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"runtime"
	"strings"
)

func init() {
	Register(Check{ID: "windows-linux-containers", Phases: Preinstall | Installed, Run: runWindowsContainers})
}

func runWindowsContainers(ctx context.Context, e *Env) Result {
	if e.Engine == nil {
		return skip("")
	}
	os := strings.ToLower(e.Engine.OSType)
	if os != "" && os != "linux" {
		return fail("Docker Desktop is set to Windows containers.", "Switch Docker Desktop to Linux containers, then run rexctl doctor.")
	}
	if runtime.GOOS != "windows" {
		return skip("")
	}
	return pass("Docker Desktop runs Linux containers.")
}
