// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"os"
	"runtime"
	"strconv"
	"strings"
)

func LowPortsBlocked(rootless bool) bool {
	if !rootless || runtime.GOOS != "linux" {
		return false
	}
	data, err := os.ReadFile("/proc/sys/net/ipv4/ip_unprivileged_port_start")
	if err != nil {
		return false
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(data)))
	return err == nil && n > 80
}
