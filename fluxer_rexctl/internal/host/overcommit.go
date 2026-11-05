// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"runtime"
	"strconv"
	"strings"
	"syscall"
)

const (
	OvercommitSetting = "vm.overcommit_memory = 1"
	OvercommitFile    = "/etc/sysctl.d/61-fluxer-overcommit.conf"
	OvercommitCommand = "echo '" + OvercommitSetting + "' | sudo tee " + OvercommitFile + " && sudo sysctl --system"
)

type OvercommitState int

const (
	OvercommitUnknown OvercommitState = iota
	OvercommitOn
	OvercommitOff
	OvercommitLocked
)

var ErrOvercommitUnchanged = errors.New("vm.overcommit_memory didn't change")

var overcommitPath = "/proc/sys/vm/overcommit_memory"

func Overcommit() OvercommitState {
	if runtime.GOOS != "linux" {
		return OvercommitUnknown
	}
	return overcommitAt(overcommitPath, os.Geteuid() == 0)
}

func overcommitAt(path string, root bool) OvercommitState {
	data, err := os.ReadFile(path)
	if err != nil {
		return OvercommitOn
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(data)))
	if err != nil || n == 1 {
		return OvercommitOn
	}
	f, err := os.OpenFile(path, os.O_WRONLY, 0)
	if err == nil {
		_ = f.Close()
		return OvercommitOff
	}
	if errors.Is(err, syscall.EROFS) || root && errors.Is(err, fs.ErrPermission) {
		return OvercommitLocked
	}
	return OvercommitOff
}

func FreeMemory() uint64 {
	if runtime.GOOS != "linux" {
		return 0
	}
	data, err := os.ReadFile("/proc/meminfo")
	if err != nil {
		return 0
	}
	var free uint64
	for _, line := range strings.Split(string(data), "\n") {
		fields := strings.Fields(line)
		if len(fields) < 2 || fields[0] != "MemAvailable:" && fields[0] != "SwapFree:" {
			continue
		}
		if kb, err := strconv.ParseUint(fields[1], 10, 64); err == nil {
			free += kb << 10
		}
	}
	return free
}

func EnableOvercommit(ctx context.Context) error {
	if _, err := output(ctx, false, "sh", "-c", "printf '"+OvercommitSetting+"\\n' > "+OvercommitFile); err != nil {
		return err
	}
	if _, err := output(ctx, false, "sysctl", "-p", OvercommitFile); err != nil {
		return err
	}
	if s := Overcommit(); s == OvercommitOff || s == OvercommitLocked {
		return fmt.Errorf("%w after sysctl -p %s", ErrOvercommitUnchanged, OvercommitFile)
	}
	return nil
}
