// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/user"
	"strconv"
	"syscall"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

var ownedCommands = map[string]bool{"backup": true, "restore": true, "postgres-upgrade": true}

func checkDataOwner(name string, euid int, data string) error {
	if !ownedCommands[name] || euid != 0 {
		return nil
	}
	fi, err := os.Stat(data)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	st, ok := fi.Sys().(*syscall.Stat_t)
	if !ok || st.Uid == 0 {
		return nil
	}
	owner := strconv.FormatUint(uint64(st.Uid), 10)
	if u, err := user.LookupId(owner); err == nil {
		owner = u.Username
	}
	return fmt.Errorf("%s belongs to %s, and running this as root would leave files %s can't open. Run it as %s, for example with sudo -u %s", data, owner, owner, owner, owner)
}

func dataOwnerCheck(name string) error {
	return checkDataOwner(name, os.Geteuid(), spec.LayoutFromEnv(os.Getenv).Data)
}
