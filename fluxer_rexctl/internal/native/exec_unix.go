// SPDX-License-Identifier: AGPL-3.0-or-later

//go:build !windows

package native

import (
	"os"
	"os/exec"
	"os/user"
	"strconv"
	"syscall"
)

func ownGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}

func asService(cmd *exec.Cmd, group bool) error {
	attr := &syscall.SysProcAttr{Setpgid: group}
	if os.Geteuid() == 0 {
		u, err := user.Lookup(ServiceUser)
		if err != nil {
			return err
		}
		uid, err := strconv.ParseUint(u.Uid, 10, 32)
		if err != nil {
			return err
		}
		gid, err := strconv.ParseUint(u.Gid, 10, 32)
		if err != nil {
			return err
		}
		var groups []uint32
		ids, _ := u.GroupIds()
		for _, id := range ids {
			if g, err := strconv.ParseUint(id, 10, 32); err == nil {
				groups = append(groups, uint32(g))
			}
		}
		attr.Credential = &syscall.Credential{Uid: uint32(uid), Gid: uint32(gid), Groups: groups}
	}
	cmd.SysProcAttr = attr
	cmd.Cancel = func() error {
		if group {
			return syscall.Kill(-cmd.Process.Pid, syscall.SIGINT)
		}
		return cmd.Process.Signal(os.Interrupt)
	}
	return nil
}
