// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"os"
	"slices"
	"strings"
)

const killAtEnv = "REXCTL_TEST_KILL_AT"

func KillPoint(point string) {
	want := os.Getenv(killAtEnv)
	if want == "" || !slices.Contains(strings.Split(want, ","), point) {
		return
	}
	if p, err := os.FindProcess(os.Getpid()); err == nil {
		_ = p.Kill()
	}
	select {}
}
