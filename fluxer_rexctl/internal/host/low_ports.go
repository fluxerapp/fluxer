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
	LowPortsSetting = "net.ipv4.ip_unprivileged_port_start = 80"
	LowPortsFile    = "/etc/sysctl.d/60-fluxer-ports.conf"
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

func AllowLowPorts(ctx context.Context) error {
	if _, err := output(ctx, false, "sh", "-c", "printf '"+LowPortsSetting+"\\n' > "+LowPortsFile); err != nil {
		return err
	}
	_, err := output(ctx, false, "sysctl", "-p", LowPortsFile)
	return err
}
