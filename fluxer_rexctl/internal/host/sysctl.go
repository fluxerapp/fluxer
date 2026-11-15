// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"fmt"
	"os"
	"strings"
)

type Sysctl struct {
	Key   string
	Value string
	File  string
	Why   string
	Now   string
	check func() error
}

var (
	OvercommitSysctl = Sysctl{
		Key:   "vm.overcommit_memory",
		Value: "1",
		File:  "/etc/sysctl.d/61-fluxer-overcommit.conf",
		Why:   "so it stays on after a restart",
		Now:   "Turns it on now.",
		check: overcommitChanged,
	}
	LowPortsSysctl = Sysctl{
		Key:   "net.ipv4.ip_unprivileged_port_start",
		Value: "80",
		File:  "/etc/sysctl.d/60-fluxer-ports.conf",
		Why:   "so programs that don't run as root can use ports 80 and 443 after a restart",
		Now:   "Applies it now.",
	}
)

func (s Sysctl) line() string { return s.Key + " = " + s.Value }

func (s Sysctl) current() string {
	data, err := os.ReadFile("/proc/sys/" + strings.ReplaceAll(s.Key, ".", "/"))
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(data))
}

func (s Sysctl) commands(sudo string) (save, apply string) {
	return "echo '" + s.line() + "' | " + sudo + "tee " + s.File, sudo + "sysctl -p " + s.File
}

func (s Sysctl) HostCommand() string {
	save, apply := s.commands("sudo ")
	return save + " && " + apply
}

func (s Sysctl) Steps() []Step {
	undo := ""
	if cur := s.current(); cur != "" {
		undo = rootCmd("sysctl -w " + s.Key + "=" + cur)
	}
	save, apply := s.commands(rootCmd(""))
	return []Step{
		{
			Do:      fmt.Sprintf("Saves %s in %s, %s.", s.line(), s.File, s.Why),
			Command: save,
			Undo:    rootCmd("rm " + s.File),
			Run: func(ctx context.Context) error {
				_, err := output(ctx, false, "sh", "-c", "printf '"+s.line()+"\\n' > "+s.File)
				return err
			},
		},
		{
			Do:      s.Now,
			Command: apply,
			Undo:    undo,
			Run: func(ctx context.Context) error {
				if _, err := output(ctx, false, "sysctl", "-p", s.File); err != nil {
					return err
				}
				if s.check != nil {
					if err := s.check(); err != nil {
						return fmt.Errorf("%w after sysctl -p %s", err, s.File)
					}
				}
				return nil
			},
		},
	}
}
