// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	handRunFromFiles  = "the compose files, which match it"
	handRunFromTag    = "FLUXER_IMAGE_TAG in .env"
	lastBundleRelease = "2026.1124.80040"
	handRunTagFix     = "Set FLUXER_IMAGE_TAG in .env to the release you run, such as " + lastBundleRelease + ", then run this again."
)

func handRunFolderKind(dir string) bool {
	switch instance.Classify(dir) {
	case instance.KindHandRunOld, instance.KindEjectedOld:
		return true
	}
	return false
}

func handRunRelease(folder *instance.HandRunFolder) (string, string, error) {
	if folder.Ejected != nil && release.Valid(folder.Ejected.Release) {
		return folder.Ejected.Release, "the record rexctl eject left in " + instance.StateDirName, nil
	}
	if tag := folder.Tag(); tag != "" {
		if !release.Valid(tag) {
			return "", "", ui.Refused(fmt.Sprintf("rexctl can't tell which files belong to %s, because it isn't a Fluxer release.", tag), handRunTagFix)
		}
		return tag, handRunFromTag, nil
	}
	return lastBundleRelease, handRunFromFiles, nil
}

func handRunSet(ctx context.Context, app *App, folder *instance.HandRunFolder, rel string) (string, func(), error) {
	if folder.Ejected != nil {
		if dir := (&instance.Instance{Dir: folder.Dir, State: folder.Ejected}).SetDir(rel); fileExists(filepath.Join(dir, "docker-compose.yml")) {
			return dir, func() {}, nil
		}
	}
	tmp, err := os.MkdirTemp("", "rexctl-bundle-")
	if err != nil {
		return "", nil, err
	}
	done := func() { os.RemoveAll(tmp) }
	dir := filepath.Join(tmp, rel, "stack")
	err = app.UI.Step(ctx, "Downloading the files of Fluxer "+rel, func(ctx context.Context, s *ui.Step) error {
		if err := release.FetchComposeBundle(ctx, rel, dir); err != nil {
			return err
		}
		s.Done("Downloaded the files of Fluxer %s.", rel)
		return nil
	})
	switch {
	case errors.Is(err, release.ErrNoBundle):
		done()
		return "", nil, ui.Refused(fmt.Sprintf("rexctl can't read this folder, because Fluxer %s didn't ship compose files.", rel), "Upgrade the folder by hand to "+lastBundleRelease+" first, then run this again.")
	case err != nil:
		done()
		return "", nil, ui.Wrap(err, fmt.Sprintf("rexctl couldn't download the files of Fluxer %s from %s.", rel, "https://github.com/fluxerapp/fluxer/releases/tag/fluxer-release@"+rel), "Check that this machine can connect to github.com, then run this again.")
	}
	return dir, done, nil
}

func runHandRun(ctx context.Context, app *App, dir string, f *migrateFlags) error {
	switch {
	case f.Undo != "", f.CleanUp, f.Migrate != "", f.OpenWrites:
		return folderRefusal(dir, &instance.FolderError{Dir: dir, Kind: instance.KindHandRunOld})
	}
	folder, err := instance.ReadHandRun(dir)
	if err != nil {
		return handRunReadError(dir, err)
	}
	e, err := app.anyEngine(ctx, &instance.Instance{Dir: dir, State: &instance.State{Engine: folder.EngineRef()}})
	if err != nil {
		return err
	}
	rel, from, err := handRunRelease(folder)
	if err != nil {
		return err
	}
	setDir, done, err := handRunSet(ctx, app, folder, rel)
	if err != nil {
		return err
	}
	defer done()
	plan, err := folder.Plan(ctx, e, rel, setDir)
	if errors.Is(err, instance.ErrHandRunEdited) && from == handRunFromFiles {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("rexctl can't tell which release the files in %s belong to.", dir), Fix: handRunTagFix, Err: err}
	}
	var edited *ui.ExitError
	if errors.Is(err, instance.ErrHandRunEdited) && from == handRunFromTag && errors.As(err, &edited) && edited.Fix != "" {
		edited.Fix = "Check that FLUXER_IMAGE_TAG in .env is the release you run. If it is, " + strings.ToLower(edited.Fix[:1]) + edited.Fix[1:]
	}
	if err != nil {
		return handRunReadError(dir, err)
	}
	defer plan.Close()
	staged := plan.Staged
	if f.To == "" {
		if f.To, err = pickMoveTarget(app, staged); err != nil {
			return err
		}
	}
	switch f.To {
	case runtimeSystemd, string(engine.Docker), string(engine.Podman), runtimeCompose:
	default:
		return ui.Usage("Pass --to as docker, podman, compose or systemd.")
	}
	if f.To == runtimeSystemd {
		if err := dockerMoveAllowed(app, staged, f.NewDir, f.DryRun); err != nil {
			return err
		}
	}
	if names := migrate.Unmovable(staged.State.Settings); len(names) > 0 {
		return ui.Refused(fmt.Sprintf("Fluxer in the new layout doesn't use %s from .env.", ui.JoinAnd(names)), fmt.Sprintf("Take %s out of .env, run docker compose up -d in %s, then run this again.", oneOrMany(len(names), "it", "them"), dir))
	}
	target := newMoveTarget(app, staged, f.To, f.NewDir, f.DryRun)
	if err := movePreflight(ctx, staged, target); err != nil {
		return err
	}
	modes := minimalModes(staged)
	items := handRunPlan(staged, f.To, target, modes)
	handRunReport(app, plan, rel, from, len(modes) > 0)
	if f.DryRun {
		app.UI.Plan("rexctl would do this", items...)
		app.UI.Info("Dry run. Nothing changed.")
		return nil
	}
	app.UI.Plan("rexctl will do this", items...)
	ok, err := confirmMove(app, staged, f.To, target.Dir())
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Info("Nothing changed.")
		return nil
	}
	inst, err := instance.RecordHandRun(plan)
	if err != nil {
		var ee *ui.ExitError
		if errors.As(err, &ee) {
			return err
		}
		return ui.Wrap(err, fmt.Sprintf("Couldn't write rexctl's record to %s. Nothing changed.", filepath.Join(dir, instance.StateDirName)), "Check that you can write to "+dir+", then run this again.")
	}
	f.confirmed = true
	err = runMove(ctx, app, inst, f)
	if err != nil && recordUnused(inst, err) {
		if uerr := instance.UndoHandRunRecord(dir); uerr != nil {
			app.UI.Detail("removing the record in %s: %v", inst.StateDir(), uerr)
		}
	}
	return err
}

func handRunReadError(dir string, err error) error {
	var ee *ui.ExitError
	if errors.As(err, &ee) {
		return err
	}
	return ui.Wrap(err, fmt.Sprintf("rexctl can't read the files in %s.", dir), "Check that you can read them, then run this again.")
}

func recordUnused(inst *instance.Instance, err error) bool {
	var ee *ui.ExitError
	if !errors.As(err, &ee) || ee.Code != ui.ExitRefused {
		return false
	}
	fresh, ferr := instance.Find(inst.Dir)
	if ferr != nil || fresh.State.MovedTo != "" || !maps.Equal(fresh.State.Settings, inst.State.Settings) {
		return false
	}
	st, serr := migrate.LoadState(inst.Dir)
	return serr == nil && st.Docker == nil && st.KV == nil && st.Storage == nil
}

func handRunPlan(inst *instance.Instance, to string, target migrate.MoveTarget, modes []migrate.Option) []ui.PlanItem {
	items := movePlan(inst, to, target.Dir(), modes, target.OneWay())
	if to == runtimeCompose && len(target.Mounts()) > 0 {
		items[len(items)-2].Detail = "the new files keep everything in Fluxer's volume, also what the old containers kept in folders on this machine"
	}
	record := ui.PlanItem{Verb: ui.PlanCreate, Text: fmt.Sprintf("Keep a record of this install in %s", filepath.Join(inst.Dir, instance.StateDirName)), Detail: "your own files stay as they are"}
	if len(modes) > 0 {
		at := 1 + len(modes)
		env := ui.PlanItem{Verb: ui.PlanChange, Text: "Update .env to match", Detail: "after the change above, for your own files to keep describing what runs. The old .env stays as " + instance.EnvBeforeRexctl}
		items = append(items[:at], append([]ui.PlanItem{env}, items[at:]...)...)
	}
	return append([]ui.PlanItem{record}, items...)
}

func keepHandRunFiles(ctx context.Context, app *App, inst *instance.Instance) error {
	if inst.State.HandRun == nil {
		return nil
	}
	changes, err := instance.HandRunEnvChanges(inst)
	var placed []string
	if err == nil {
		placed, err = instance.SyncHandRunEnv(inst, changes, time.Now())
	}
	if err == nil {
		err = inst.SameAsFiles(ctx)
	}
	if err != nil {
		msg := fmt.Sprintf("rexctl stopped before the move, because your files in %s don't match what runs yet.", inst.Dir)
		fix := "Fluxer keeps running."
		if fileExists(inst.Path(instance.EnvBeforeRexctl)) {
			fix += fmt.Sprintf(" Your earlier .env is %s.", inst.Path(instance.EnvBeforeRexctl))
		}
		var ee *ui.ExitError
		if errors.As(err, &ee) {
			msg += " " + ee.Msg
			if ee.Fix != "" {
				fix += " " + ee.Fix
			}
		}
		return &ui.ExitError{Code: ui.ExitFailed, Msg: msg, Fix: fix, Err: err}
	}
	if len(changes) > 0 {
		app.UI.OK("Updated .env in %s to match what runs. The earlier one is %s.", inst.Dir, instance.EnvBeforeRexctl)
	}
	if len(placed) > 0 {
		app.UI.Info("rexctl added %s from Fluxer %s to %s, because .env now lists %s.", ui.JoinAnd(placed), inst.State.Release, inst.Dir, oneOrMany(len(placed), "it", "them"))
	}
	return nil
}

func handRunReport(app *App, plan *instance.HandRunPlan, rel, from string, modes bool) {
	folder := plan.Folder
	body := []string{
		fmt.Sprintf("The release is %s, which rexctl read from %s.", rel, from),
		fmt.Sprintf("The compose files are %s.", ui.JoinAnd(folder.Files)),
		fmt.Sprintf("rexctl keeps %d settings from .env.", plan.Kept),
	}
	if len(plan.Dropped) > 0 {
		body = append(body, fmt.Sprintf("rexctl leaves out %s, because Fluxer doesn't read %s.", ui.JoinAnd(plan.Dropped), oneOrMany(len(plan.Dropped), "it", "them")))
	}
	if len(plan.Unknown) > 0 {
		body = append(body, fmt.Sprintf("%s %s new to rexctl, and rexctl keeps %s as %s.", ui.JoinAnd(plan.Unknown), oneOrMany(len(plan.Unknown), "is", "are"), oneOrMany(len(plan.Unknown), "it", "them"), oneOrMany(len(plan.Unknown), "it is", "they are")))
	}
	if len(plan.Minted) > 0 {
		body = append(body, fmt.Sprintf("rexctl creates %s, which .env doesn't set.", ui.JoinAnd(plan.Minted)))
	}
	files := "Your own files stay as they are."
	if modes {
		files = "Your own files stay as they are, apart from .env, which the plan below updates."
	}
	body = append(body, fmt.Sprintf("rexctl adds %s and puts backups in %s. %s", filepath.Join(folder.Dir, instance.StateDirName), plan.Staged.BackupDir(), files))
	app.UI.Card(ui.Card{Title: "Fluxer in " + folder.Dir + " was set up by hand", Body: body})
}

func handRunUndoRefusal(inst *instance.Instance) error {
	if err := handRunChangedRefusal(inst); err != nil || inst.State.HandRun == nil {
		return err
	}
	now, err := os.ReadFile(inst.Path(instance.EnvFileName))
	if err != nil {
		return err
	}
	if before, err := os.ReadFile(inst.Path(instance.EnvBeforeRexctl)); err == nil && !bytes.Equal(before, now) {
		return ui.Refused(fmt.Sprintf("rexctl updated .env in %s for this change, and going back would leave .env out of step with what runs.", inst.Dir), "Fluxer keeps running as it is. Move it with rexctl migrate, or remove the record with rexctl migrate --forget.")
	}
	return nil
}

func handRunChangedRefusal(inst *instance.Instance) error {
	name := instance.HandRunChanged(inst)
	if name == "" {
		return nil
	}
	return ui.Refused(fmt.Sprintf("rexctl won't act on the old containers, because %s changed after rexctl recorded this install.", name), fmt.Sprintf("Put the file back as it was. A copy is in %s.", inst.HandRunCopies()))
}

func handRunCleanedUp(app *App, old *instance.Instance) {
	if old == nil || old.State.HandRun == nil {
		return
	}
	if err := instance.RemoveHandRunRecord(old.Dir); err != nil {
		app.UI.Detail("%v", err)
		app.UI.Warn(fmt.Sprintf("Couldn't remove rexctl's record from %s.", old.Dir), "Delete "+old.StateDir()+" yourself.")
		return
	}
	app.forget(old.Dir)
	app.UI.Info("The old containers are gone. rexctl removed its record from %s. Your own files there are no longer in use, and you can delete the folder.", old.Dir)
}
