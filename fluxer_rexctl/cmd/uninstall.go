// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newUninstallCmd(app *App) *cobra.Command {
	var keepData bool
	var confirm string
	c := &cobra.Command{
		Use:   "uninstall",
		Short: "Remove Fluxer from this machine",
		Args:  noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runUninstall(c.Context(), app, keepData, confirm)
		},
	}
	c.Flags().BoolVar(&keepData, "keep-data", false, "keep your data and backups")
	confirmFlag(c, &confirm)
	return c
}

func runUninstall(ctx context.Context, app *App, keepData bool, confirm string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	p, err := app.Named(ctx, inst)
	if err != nil {
		return err
	}
	if !keepData {
		what := "This deletes every account, message and file on your instance, and its backups."
		if err := app.UI.ConfirmDestructive(what, inst.Domain(), confirm); err != nil {
			return err
		}
	} else if ok, err := app.UI.Confirm(fmt.Sprintf("Stop and remove Fluxer? Your data stays in %s.", inst.Dir), false); err != nil {
		return err
	} else if !ok {
		return ui.Refused("Stopped without changing anything.", "")
	}
	var images []string
	err = app.Locked(ctx, inst, func() error {
		return app.UI.Step(ctx, "Removing Fluxer", func(ctx context.Context, s *ui.Step) error {
			if !keepData {
				images = slices.Sorted(maps.Values(inst.State.Images.Refs))
				for _, r := range release.KeptImages(inst) {
					if !slices.Contains(images, r) {
						images = append(images, r)
					}
				}
			}
			if err := p.Down(context.WithoutCancel(ctx), !keepData); err != nil {
				return engineFailure(err)
			}
			if err := removeBootUnit(ctx, inst, p.Engine); err != nil {
				app.UI.Detail("%v", err)
			}
			if _, err := removeAutoUpgrade(ctx, inst, p.Engine); err != nil {
				app.UI.Detail("removing automatic upgrades: %v", err)
			}
			s.Done("Removed Fluxer.")
			return nil
		})
	})
	var ee *ui.ExitError
	if err != nil && !errors.As(err, &ee) {
		return ui.Wrap(err, "Couldn't remove Fluxer.", "Run rexctl doctor to see what's wrong.")
	}
	if err != nil {
		return err
	}
	app.forget(inst.Dir)
	if keepData {
		app.UI.Info("Your data and backups are still in %s.", inst.Dir)
		return nil
	}
	if len(images) > 0 {
		if err := p.Engine.RemoveImages(ctx, images); err != nil {
			app.UI.Detail("%v", err)
		}
	}
	leaveDir(inst.Dir)
	if err := os.RemoveAll(inst.Dir); err != nil {
		if entries, rerr := os.ReadDir(inst.Dir); rerr != nil || len(entries) > 0 {
			return ui.Wrap(err, fmt.Sprintf("Couldn't delete %s.", inst.Dir), "Delete the folder yourself.")
		}
		app.UI.OK("Deleted your instance and its data.")
		app.UI.Info("Your terminal is still in %s. Delete the empty folder after you leave it.", inst.Dir)
		return nil
	}
	app.UI.OK("Deleted your instance and its data.")
	return nil
}

func leaveDir(dir string) {
	wd, err := os.Getwd()
	if err != nil {
		return
	}
	rel, err := filepath.Rel(dir, wd)
	if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return
	}
	_ = os.Chdir(filepath.Dir(dir))
}
