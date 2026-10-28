// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"errors"
	"fmt"
	"path/filepath"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newUseCmd(app *App) *cobra.Command {
	return &cobra.Command{
		Use:   "use [folder]",
		Short: "Show or change which instance rexctl uses",
		Long: "Show which instance rexctl uses when you don't pass --dir and aren't in an instance folder, or pass a folder to use that instance from now on. " +
			"rexctl remembers each instance it installs, so you only need this with more than one instance or one installed by an older rexctl. " +
			"Each account has its own choice. Root's choice also applies to accounts that have no instance of their own. " +
			"The services rexctl sets up for starting and upgrading Fluxer always name their folder, so this doesn't change them.",
		Args: maxArgs(1),
		RunE: func(c *cobra.Command, args []string) error {
			if len(args) == 0 {
				return runUseShow(app)
			}
			return runUse(app, args[0])
		},
	}
}

func runUseShow(app *App) error {
	l, err := instance.Resolve(app.Flags.Dir)
	var several *instance.SeveralError
	switch {
	case errors.As(err, &several):
		app.UI.Info("This machine has Fluxer instances in %s, and none of them is the default.", ui.JoinAnd(several.Dirs))
		app.UI.Info("Run rexctl use <folder> to pick one.")
		return nil
	case err != nil:
		return untrustedRefusal(err)
	}
	if l.Stale != "" {
		app.warnStale(l)
	}
	if _, err := instance.Find(l.Dir); errors.Is(err, instance.ErrNotFound) {
		app.UI.Info("There's no Fluxer instance in %s (%s).", l.Dir, l.Why())
		app.UI.Info("Run rexctl use <folder> with the folder Fluxer is in.")
		return nil
	}
	app.UI.Result(fmt.Sprintf("Using %s (%s).", l.Dir, l.Why()))
	return nil
}

func runUse(app *App, arg string) error {
	dir, err := filepath.Abs(arg)
	if err != nil {
		return err
	}
	switch _, err := instance.Find(dir); {
	case err == nil, errors.Is(err, instance.ErrLegacy):
	case errors.Is(err, instance.ErrNotFound):
		return ui.Refused(fmt.Sprintf("There's no Fluxer instance in %s.", dir), "Pass the folder your instance is in.")
	default:
		return ui.Wrap(err, fmt.Sprintf("rexctl can't read the instance in %s.", dir), "Run rexctl as the account that installed Fluxer.")
	}
	s := instance.OwnScope()
	if err := instance.Remember(s, dir, true); err != nil {
		return ui.Wrap(untrustedRefusal(err), fmt.Sprintf("Couldn't save %s.", instance.RecordPath(s)), "Check that you can write to it, then try again.")
	}
	if s == instance.ScopeMachine {
		app.UI.OK("rexctl uses the instance in %s from now on, for root and for accounts that have no instance of their own.", dir)
	} else {
		app.UI.OK("rexctl uses the instance in %s for your account from now on.", dir)
	}
	return nil
}
