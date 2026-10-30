// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"fmt"
	"io/fs"
	"os/user"
	"strconv"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func checkOwnership(app *App, inst *instance.Instance) error {
	if euid() == 0 {
		if err := inst.GiveFilesToOwner(); err != nil {
			app.UI.Detail("rexctl couldn't give every file in %s back to the account that owns %s: %v", inst.StateDir(), inst.Dir, err)
		}
		return nil
	}
	f, ok := inst.ForeignFile()
	if !ok {
		return nil
	}
	return ui.Refused(
		fmt.Sprintf("rexctl can't change %s. It belongs to %s and rexctl runs as %s.", f.Path, accountName(f.Owner), accountName(euid())),
		chownFix(accountName(euid()), f.Dir)+", then try again.",
	)
}

func giveFilesBack(app *App, inst *instance.Instance) {
	if euid() != 0 {
		return
	}
	if err := inst.GiveFilesToOwner(); err != nil {
		app.UI.Detail("rexctl couldn't give every file in %s back to the account that owns %s: %v", inst.StateDir(), inst.Dir, err)
	}
}

func saveFailed(inst *instance.Instance, err error, msg, command string) error {
	again := "run " + command + " again."
	switch {
	case noSpace(err):
		return ui.Wrap(err, msg, "The disk is full. Free up space, then "+again)
	case errors.Is(err, fs.ErrPermission):
		who := accountName(euid())
		if euid() == 0 {
			if uid, ok := inst.OwnerUID(); ok {
				who = accountName(uid)
			}
		}
		path, dir := inst.StateDir(), inst.StateDir()
		if f, ok := inst.ForeignFile(); ok {
			path, dir = f.Path, f.Dir
		} else {
			var pe *fs.PathError
			if errors.As(err, &pe) {
				path = pe.Path
			}
		}
		return ui.Wrap(err, msg, fmt.Sprintf("rexctl isn't allowed to write %s. %s, then %s", path, chownFix(who, dir), again))
	}
	return ui.Wrap(err, msg, fmt.Sprintf("Run %s --verbose to see why.", command))
}

func chownFix(account, dir string) string {
	return fmt.Sprintf("Run sudo chown -R %s: %s", account, dir)
}

func accountName(uid int) string {
	id := strconv.Itoa(uid)
	if u, err := user.LookupId(id); err == nil && u.Username != "" {
		return u.Username
	}
	return id
}
