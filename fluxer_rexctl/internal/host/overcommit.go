// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"os"
	"runtime"
	"strconv"
	"strings"
)

const (
	OvercommitSetting = "vm.overcommit_memory = 1"
	OvercommitFile    = "/etc/sysctl.d/61-fluxer-overcommit.conf"
)

func OvercommitDisabled() bool {
	if runtime.GOOS != "linux" {
		return false
	}
	data, err := os.ReadFile("/proc/sys/vm/overcommit_memory")
	if err != nil {
		return false
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(data)))
	return err == nil && n != 1
}

func EnableOvercommit(ctx context.Context) error {
	if _, err := output(ctx, false, "sh", "-c", "printf '"+OvercommitSetting+"\\n' > "+OvercommitFile); err != nil {
		return err
	}
	_, err := output(ctx, false, "sysctl", "-p", OvercommitFile)
	return err
}
