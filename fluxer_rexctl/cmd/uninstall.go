// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func newUninstallCmd(app *App) *cobra.Command {
	var keepData, deleteBackups bool
	var confirm string
	c := &cobra.Command{
		Use:     "uninstall",
		Short:   "Remove Fluxer from this machine",
		Long:    "Remove Fluxer from this machine, with its data unless you pass --keep-data. rexctl asks whether to keep your backups, and they stay unless you choose to delete them or pass --delete-backups. It lists what goes and what stays before it changes anything. If you only want to stop Fluxer and keep everything, run rexctl stop or rexctl down.",
		Example: "rexctl uninstall\nrexctl uninstall --keep-data\nrexctl uninstall --confirm chat.example.com --delete-backups",
		Args:    noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runUninstall(c.Context(), app, keepData, deleteBackups, confirm)
		},
	}
	c.Flags().BoolVar(&keepData, "keep-data", false, "keep your data, settings and backups")
	c.Flags().BoolVar(&deleteBackups, "delete-backups", false, "delete the backups in the instance folder too")
	c.MarkFlagsMutuallyExclusive("keep-data", "delete-backups")
	confirmFlag(c, &confirm)
	return c
}

func runUninstall(ctx context.Context, app *App, keepData, deleteBackups bool, confirm string) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.IsVoice() {
		return runVoiceMachineUninstall(ctx, app, inst)
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return err
	}
	if r.Info(ctx).Root {
		if ok, err := app.asRoot(inst.Dir); err != nil || !ok {
			return err
		}
	}
	o := rt.UninstallOptions{KeepData: keepData}
	backups := backupsInside(inst.BackupDir(), inst.Dir)
	if backups && !keepData {
		if o.KeepBackups, err = keepBackups(app, inst.BackupDir(), deleteBackups); err != nil {
			return err
		}
	}
	plan := rt.PlanUninstall(ctx, bare(r), o)
	if r.Kind() == rt.Compose && rt.UninstallEngine(bare(r)) == nil {
		if kind := engine.Kind(inst.State.Engine.Kind); engineOnMachine(kind) {
			return ui.Refused(fmt.Sprintf("rexctl can't connect to %s, so it can't stop and remove Fluxer's containers.", kind.Name()), fmt.Sprintf("Start %s, then run rexctl uninstall again.", kind.Name()))
		}
	}
	if plan.Account != "" {
		o.RemoveUser = !keepData
		if keepData {
			plan.Stays = append(plan.Stays, ui.KV{Key: "Account", Value: plan.Account})
		} else {
			plan.Goes = append(plan.Goes, ui.KV{Key: "Account", Value: plan.Account})
		}
	}
	if kits, _ := filepath.Glob(filepath.Join(inst.Dir, "recovery-kit-*.txt")); len(kits) > 0 && !keepData {
		var names []string
		for _, k := range kits {
			names = append(names, filepath.Base(k))
		}
		plan.Goes = append(plan.Goes, ui.KV{Key: "Recovery kit", Value: strings.Join(names, ", ")})
	}
	if bin := runningBinary(); bin != "" {
		plan.Stays = append(plan.Stays, ui.KV{Key: "rexctl", Value: bin})
	}
	for _, o := range routedThrough(inst) {
		app.UI.Warn(fmt.Sprintf("The Fluxer in %s goes through this one, and %s stops working once this one is gone.", o.Dir, o.Domain()), "Afterwards, run rexctl proxy on --dir "+shellWord(o.Dir)+" and point a proxy at it.")
	}
	if !keepData {
		card := ui.DestructiveCard{Title: "Remove Fluxer from " + inst.Domain(), Goes: plan.Goes, Stays: plan.Stays}
		if err := app.UI.Destructive(card, inst.Domain(), confirm); err != nil {
			return err
		}
	} else {
		var items []ui.PlanItem
		for _, kv := range plan.Goes {
			items = append(items, ui.PlanItem{Verb: ui.PlanRemove, Text: kv.Key, Detail: kv.Value})
		}
		for _, kv := range plan.Stays {
			items = append(items, ui.PlanItem{Verb: ui.PlanKeep, Text: kv.Key, Detail: kv.Value})
		}
		app.UI.Plan("Remove Fluxer from "+inst.Domain(), items...)
		ok, err := app.UI.ConfirmWith(ui.Question{Text: "Stop and remove Fluxer and keep your data?"})
		if err != nil {
			return err
		}
		if !ok {
			app.UI.Info("Nothing changed.")
			return nil
		}
	}
	err = app.Locked(ctx, inst, func() error {
		return app.UI.Step(ctx, "Removing Fluxer", func(ctx context.Context, s *ui.Step) error {
			if err := r.Uninstall(ctx, s, o); err != nil {
				return err
			}
			unrouteFromEdge(context.WithoutCancel(ctx), app, inst)
			if _, err := removeAutoUpgrade(context.WithoutCancel(ctx), inst, rt.UninstallEngine(bare(r))); err != nil {
				app.UI.Detail("removing automatic upgrades: %v", err)
				s.Warn("The timer that upgrades Fluxer by itself is still on this machine.", "")
			}
			if err := removeBackupSchedule(context.WithoutCancel(ctx), inst); err != nil && !errors.Is(err, host.ErrNoScheduler) {
				app.UI.Detail("removing scheduled backups: %v", err)
				s.Warn("The timer that backs up Fluxer by itself is still on this machine.", "")
			}
			s.Done("Removed Fluxer.")
			return nil
		})
	})
	if err != nil {
		return err
	}
	app.forget(inst.Dir)
	if keepData {
		if plan.Kept != "" {
			app.UI.Info("%s", plan.Kept)
		}
		return nil
	}
	done := "Deleted your instance and its data."
	if rt.DataLeft(bare(r)) {
		done = "Deleted your instance. Some of its data is still on this machine, as listed above."
	}
	if o.KeepBackups {
		if err := removeAllBut(inst.Dir, inst.BackupDir()); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't delete %s.", inst.Dir), fmt.Sprintf("Delete everything in it except %s yourself.", inst.BackupDir()))
		}
		app.UI.OK("%s", done)
		app.UI.Info("Your backups are still in %s. %s", inst.BackupDir(), bringBack(r, inst))
		return nil
	}
	leaveDir(inst.Dir)
	if err := os.RemoveAll(inst.Dir); err != nil {
		if entries, rerr := os.ReadDir(inst.Dir); rerr != nil || len(entries) > 0 {
			return ui.Wrap(err, fmt.Sprintf("Couldn't delete %s.", inst.Dir), "Delete the folder yourself.")
		}
		app.UI.OK("%s", done)
		app.UI.Info("Your terminal is still in %s. Delete the empty folder after you leave it.", inst.Dir)
		return nil
	}
	app.UI.OK("%s", done)
	app.UI.Rex("All cleaned up. Run rexctl install whenever you want Fluxer back.")
	return nil
}

func backupsInside(backups, dir string) bool {
	rel, err := filepath.Rel(dir, backups)
	if err != nil || rel == "." || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return false
	}
	entries, err := os.ReadDir(backups)
	return err == nil && len(entries) > 0
}

func keepBackups(app *App, dir string, deleteThem bool) (bool, error) {
	switch {
	case deleteThem:
		return false, nil
	case app.UI.Yes() || !app.UI.Interactive():
		return true, nil
	}
	return app.UI.ConfirmWith(ui.Question{Text: "Keep the backups in " + dir + "?", Detail: "You can restore Fluxer from them later.", Default: true})
}

func removeAllBut(dir, keep string) error {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return err
	}
	for _, e := range entries {
		path := filepath.Join(dir, e.Name())
		if path == filepath.Clean(keep) {
			continue
		}
		if inner, err := filepath.Rel(path, keep); err == nil && inner != ".." && !strings.HasPrefix(inner, ".."+string(filepath.Separator)) {
			if err := removeAllBut(path, keep); err != nil {
				return err
			}
			continue
		}
		if err := os.RemoveAll(path); err != nil {
			return err
		}
	}
	return nil
}

func engineOnMachine(kind engine.Kind) bool {
	if kind == engine.Podman {
		_, err := exec.LookPath("podman")
		return err == nil
	}
	return dockerInstalled()
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

func bringBack(r rt.Runtime, inst *instance.Instance) string {
	switch r.Kind() {
	case rt.Native:
		return "Run sudo rexctl restore with one of them to bring Fluxer back."
	case rt.Container:
		kind := engine.Kind(inst.State.Engine.Kind)
		if kind == "" {
			kind = engine.Docker
		}
		rexctl := "rexctl"
		if ranWithSudo() {
			rexctl = "sudo rexctl"
		}
		return fmt.Sprintf("To bring Fluxer back, run %s install --runtime %s, then %s restore with one of them.", rexctl, kind, rexctl)
	}
	return "Run rexctl restore with one of them to bring Fluxer back."
}
