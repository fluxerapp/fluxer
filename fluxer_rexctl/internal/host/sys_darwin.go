// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"time"

	"golang.org/x/sys/unix"
)

func Memory() (uint64, error) { return unix.SysctlUint64("hw.memsize") }

func FreeDisk(path string) (uint64, error) {
	var st unix.Statfs_t
	if err := unix.Statfs(existingParent(path), &st); err != nil {
		return 0, err
	}
	return st.Bavail * uint64(st.Bsize), nil
}

func DiskID(path string) (string, error) {
	var st unix.Stat_t
	if err := unix.Stat(existingParent(path), &st); err != nil {
		return "", err
	}
	return strconv.FormatInt(int64(st.Dev), 10), nil
}

func addrInUse(err error) bool { return errors.Is(err, syscall.EADDRINUSE) }

func portInUse(proto string, port int) (bool, string, error) {
	used, err := bindTest(proto, port)
	if err != nil || !used {
		return used, "", err
	}
	return true, lsofOwner(proto, port), nil
}

func lsofOwner(proto string, port int) string {
	if !has("lsof") {
		return ""
	}
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	args := []string{"-nP", "-F", "c"}
	if proto == "tcp" {
		args = append(args, fmt.Sprintf("-iTCP:%d", port), "-sTCP:LISTEN")
	} else {
		args = append(args, fmt.Sprintf("-iUDP:%d", port))
	}
	out, _ := exec.CommandContext(ctx, "lsof", args...).Output()
	s := bufio.NewScanner(bytes.NewReader(out))
	for s.Scan() {
		if name, ok := strings.CutPrefix(s.Text(), "c"); ok && name != "" {
			return name
		}
	}
	return ""
}
