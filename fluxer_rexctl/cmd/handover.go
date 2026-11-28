// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const handOverCheckTimeout = 10 * time.Minute

type handOverOptions struct {
	Move     bool
	Schedule string
}

type handedOver struct {
	out      rt.Detached
	tool     string
	schedule string
}

func handOverRefusal(inst *instance.Instance, move bool) error {
	if repo := rt.ImageRepoInUse(); repo != rt.ImageRepo {
		fix := "Unset REXCTL_IMAGE_REPO, then run this again."
		if move {
			fix = "Unset REXCTL_IMAGE_REPO, or keep rexctl running it with --to docker."
		}
		return ui.Refused(fmt.Sprintf("This instance runs the image %s, and the admin panel's upgrade command only works with %s.", repo, rt.ImageRepo), fix)
	}
	if len(inst.State.Voice) > 0 {
		ids := make([]string, len(inst.State.Voice))
		for i, v := range inst.State.Voice {
			ids[i] = v.ID
		}
		return ui.Refused(fmt.Sprintf("The voice %s %s %s in a container on this machine, and nothing would restart or upgrade %s after the hand-over.", oneOrMany(len(ids), "server", "servers"), ui.JoinAnd(ids), oneOrMany(len(ids), "runs", "run"), oneOrMany(len(ids), "it", "them")), "Remove "+oneOrMany(len(ids), "it", "them")+" with rexctl voice remove, then run this again.")
	}
	return nil
}

func settingsFitEnvFile(values map[string]string) error {
	for k, v := range values {
		if !rt.EnvQuotable(v) {
			return ui.Refused(fmt.Sprintf("The setting %s holds characters that fluxer.env can't hold together.", k), "Change that setting with rexctl config set, then run this again.")
		}
	}
	return nil
}

func handOver(ctx context.Context, app *App, inst *instance.Instance, o handOverOptions) (handedOver, error) {
	u := app.UI
	res := handedOver{tool: instance.ComposeTool(inst.State.Engine.Kind)}
	if err := handOverRefusal(inst, o.Move); err != nil {
		return res, err
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return res, err
	}
	sched, schedErr := backupScheduleFor(r, inst)
	do := rt.DetachOptions{Rexctl: app.Version, Schedule: host.ScheduleOff, Keep: inst.KeepBackups()}
	if schedErr == nil {
		do.Schedule = sched.Current()
	}
	if o.Schedule != "" {
		do.Schedule = o.Schedule
	}
	if do.Schedule == host.ScheduleOff {
		do.Schedule = fluxerdSchedule(inst)
	}
	res.schedule = do.Schedule
	if inst.State.MovedFrom != nil {
		inst.State.MovedFrom.Then = instance.ThenCompose
	}
	tool := res.tool
	err = u.Step(ctx, "Handing Fluxer over to "+tool, func(ctx context.Context, s *ui.Step) error {
		var err error
		var unquotable *rt.UnquotableError
		res.out, err = rt.Detach(ctx, r, do)
		switch {
		case errors.Is(err, rt.ErrMultiline):
			return ui.Refused("A setting of this instance holds more than one line, which an env file can't hold.", "Change that setting with rexctl config set, then run this again.")
		case errors.As(err, &unquotable):
			return ui.Refused(fmt.Sprintf("The setting %s holds characters that fluxer.env can't hold together.", unquotable.Key), "Change that setting with rexctl config set, then run this again.")
		case errors.Is(err, rt.ErrDetachStartedAgain):
			u.Detail("%v", err)
			if o.Move {
				return handOverFailed(err, tool, inst)
			}
			return rt.ContainerError(err, "Couldn't hand Fluxer over to "+tool+". rexctl started its own container again.")
		case errors.Is(err, rt.ErrDetachLeftStopped):
			u.Detail("%v", err)
			if o.Move {
				fix := fmt.Sprintf("Run rexctl start --dir %s, then rexctl detach --dir %s to try again, or rexctl migrate --undo --dir %s to go back.", shellWord(inst.Dir), shellWord(inst.Dir), shellWord(inst.State.MovedFrom.Dir))
				return &ui.ExitError{Code: ui.ExitFailed, Msg: "The move worked, but rexctl couldn't hand Fluxer over to " + tool + " and couldn't start its own container again.", Fix: fix, Err: err}
			}
			return rt.ContainerError(err, "Couldn't hand Fluxer over to "+tool+", and couldn't start rexctl's own container again. Run rexctl start --dir "+shellWord(inst.Dir)+".")
		case err != nil:
			u.Detail("%v", err)
			if o.Move {
				return handOverFailed(err, tool, inst)
			}
			return rt.ContainerError(err, "Couldn't hand Fluxer over to "+tool+". Nothing changed, and Fluxer keeps running.")
		}
		s.Done("Fluxer runs from %s.", res.out.ComposeFile)
		return nil
	})
	if err != nil {
		return res, err
	}
	if res.out.EnvLeft {
		u.Detail("couldn't remove %s from Fluxer's volume", rt.ContainerData+"/"+rt.EnvFileName)
	}
	if e, err := app.anyEngine(ctx, inst); err == nil {
		if _, err := removeAutoUpgrade(ctx, inst, e); err != nil {
			u.Detail("%v", err)
		}
	}
	if schedErr == nil {
		if _, err := sched.Remove(ctx); err != nil {
			u.Detail("%v", err)
		}
	}
	return res, nil
}

func handOverFailed(err error, tool string, inst *instance.Instance) error {
	fix := fmt.Sprintf("Run rexctl detach --dir %s to try again, or rexctl migrate --undo --dir %s to go back.", shellWord(inst.Dir), shellWord(inst.State.MovedFrom.Dir))
	return &ui.ExitError{Code: ui.ExitFailed, Msg: fmt.Sprintf("The move worked, but rexctl couldn't hand Fluxer over to %s. Fluxer runs in rexctl's container.", tool), Fix: fix, Err: err}
}

func stepAway(app *App, inst *instance.Instance) {
	for _, dir := range []string{inst.StateDir(), rt.ConfigDir(inst)} {
		if err := os.RemoveAll(dir); err != nil {
			app.UI.Detail("removing %s: %v", dir, err)
		}
	}
	app.forget(inst.Dir)
}

func checkComposed(ctx context.Context, app *App, inst *instance.Instance, out rt.Detached) error {
	return app.UI.Check(ctx, "Checking that it works again", func(ctx context.Context, s *ui.Step) error {
		e, err := app.anyEngine(ctx, inst)
		if err != nil {
			return err
		}
		p, err := e.LoadPublished(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: []string{out.ComposeFile}})
		if err != nil {
			return err
		}
		if err := p.WaitReady(ctx, handOverCheckTimeout, nil); err != nil {
			return err
		}
		if err := answers(ctx, inst, handOverCheckTimeout); err != nil {
			return err
		}
		s.Done("Fluxer runs from %s and responds on this machine.", out.ComposeFile)
		return nil
	})
}

func answers(ctx context.Context, inst *instance.Instance, timeout time.Duration) error {
	urls := healthURLs(inst)
	client := &http.Client{
		Timeout: 10 * time.Second,
		Transport: &http.Transport{
			Proxy:           nil,
			TLSClientConfig: &tls.Config{ServerName: settings.BareHost(inst.Domain()), InsecureSkipVerify: true},
		},
		CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse },
	}
	deadline := time.Now().Add(timeout)
	for {
		for _, u := range urls {
			req, err := http.NewRequestWithContext(ctx, http.MethodGet, u, nil)
			if err != nil {
				continue
			}
			req.Host = inst.Domain()
			resp, err := client.Do(req)
			if err != nil {
				continue
			}
			resp.Body.Close()
			if resp.StatusCode < 400 {
				return nil
			}
		}
		if time.Now().After(deadline) {
			return fmt.Errorf("fluxer doesn't respond on %s", strings.Join(urls, " or "))
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(5 * time.Second):
		}
	}
}

func healthURLs(inst *instance.Instance) []string {
	if mode := inst.State.Settings[settings.EdgeMode]; mode != "" && mode != settings.EdgeBundled {
		bind, err := native.ProxyListen(inst.State.Settings[settings.EdgeBind])
		if err != nil {
			bind = settings.DefaultBind
		}
		host, port, _ := net.SplitHostPort(bind)
		if host == "" || host == "0.0.0.0" || host == "::" {
			host = "127.0.0.1"
		}
		return []string{"http://" + net.JoinHostPort(host, port) + "/_health"}
	}
	https := cmpDir(inst.Setting("FLUXER_HTTPS_PORT"), "443")
	http := cmpDir(inst.Setting("FLUXER_HTTP_PORT"), "80")
	return []string{"https://" + net.JoinHostPort("127.0.0.1", https) + "/_health", "http://" + net.JoinHostPort("127.0.0.1", http) + "/_health"}
}

func confirmMove(app *App, inst *instance.Instance, to, dir string) (bool, error) {
	if to != runtimeCompose {
		return app.UI.Confirm("Make these changes?", false)
	}
	return app.UI.ConfirmWith(ui.Question{
		Text:   fmt.Sprintf("Move Fluxer to %s?", instance.ComposeTool(string(composeEngine(inst)))),
		Detail: fmt.Sprintf("rexctl backs up Fluxer, moves it into %s, checks it, then writes compose.yaml and fluxer.env there and starts Fluxer from them. The old containers in %s stay stopped with their data until you run rexctl migrate --clean-up, and rexctl migrate --undo goes back until then. Automatic upgrades stop, and scheduled backups move into Fluxer.", dir, inst.Dir),
		Flag:   "yes",
	})
}

func resumeHandOver(ctx context.Context, app *App, old *instance.Instance, dryRun bool) (bool, error) {
	if old.State.MovedTo == "" {
		return false, nil
	}
	moved, err := instance.Find(old.State.MovedTo)
	if err != nil || !moved.State.MovedFrom.HandsOver() || moved.State.Ejected != nil || instance.RealPath(moved.State.MovedFrom.Dir) != instance.RealPath(old.Dir) {
		return false, nil
	}
	tool := instance.ComposeTool(moved.State.Engine.Kind)
	app.UI.Info("The move to %s is done, but the hand-over to %s isn't.", moved.Dir, tool)
	if dryRun {
		app.UI.Plan("rexctl would do this",
			ui.PlanItem{Verb: ui.PlanCreate, Text: fmt.Sprintf("Write compose.yaml and fluxer.env to %s and start Fluxer from them with %s", moved.Dir, tool)},
			ui.PlanItem{Verb: ui.PlanRun, Text: "Check that it works again"},
		)
		app.UI.Info("Dry run. Nothing changed.")
		return true, nil
	}
	return true, finishHandOver(ctx, app, old, moved, readSchedules(ctx, old))
}

func finishHandOver(ctx context.Context, app *App, old, moved *instance.Instance, s schedules) error {
	var res handedOver
	err := app.Locked(ctx, moved, func() error {
		var err error
		res, err = handOver(ctx, app, moved, handOverOptions{Move: true, Schedule: s.backups})
		return err
	})
	if err := dropSchedules(ctx, old, s); err != nil {
		app.UI.Detail("removing the schedules of %s: %v", old.Dir, err)
	}
	moved.State.MovedFrom.AutoUpgrade = moved.State.MovedFrom.AutoUpgrade || s.auto
	if err != nil {
		if moved.State.Ejected == nil {
			applySchedules(ctx, app, moved, s)
		}
		if serr := saveInstance(moved); serr != nil {
			app.UI.Detail("%v", serr)
		}
		return err
	}
	if err := saveInstance(moved); err != nil {
		return err
	}
	if err := checkComposed(ctx, app, moved, res.out); err != nil {
		return takeBack(ctx, app, moved, res, s, err)
	}
	handOverCard(app, old, moved, res)
	return nil
}

func takeBack(ctx context.Context, app *App, moved *instance.Instance, res handedOver, s schedules, cause error) error {
	app.UI.Detail("%v", cause)
	failed := handOverFailed(cause, res.tool, moved)
	r, err := app.Runtime(ctx, moved)
	if err == nil {
		_, err = rt.TakeBack(ctx, r, res.out)
	}
	if err == nil {
		moved.State.MovedFrom.Then = ""
		err = saveInstance(moved)
	}
	if err == nil {
		err = migrateStart(ctx, app, moved)
	}
	if err == nil {
		applySchedules(ctx, app, moved, s)
	}
	if err != nil {
		app.UI.Detail("%v", err)
		return rt.ContainerError(errors.Join(cause, err), "Couldn't hand Fluxer over to "+res.tool+", and couldn't start rexctl's own container again. Run rexctl start --dir "+shellWord(moved.Dir)+".")
	}
	return failed
}

func handOverCard(app *App, old, moved *instance.Instance, res handedOver) {
	body := []string{
		fmt.Sprintf("Your files are compose.yaml and fluxer.env in %s. rexctl doesn't change this instance.", moved.Dir),
		fmt.Sprintf("The old containers in %s are stopped and still have their data. Don't start them while you try the new one.", old.Dir),
	}
	if len(res.out.Differs) == 0 {
		body = append(body, fmt.Sprintf("compose.yaml is the file from Fluxer %s with the line name: %s added. The upgrade steps in the docs work with it as written.", moved.State.Release, moved.Project()))
	} else {
		body = append(body, fmt.Sprintf("compose.yaml differs from the release file in %s. Keep this file and change only its image line to upgrade.", ui.JoinAnd(res.out.Differs)))
	}
	body = append(body, hostCopyLines(moved, res.tool)...)
	body = append(body, "When your messages and uploads are there, remove the old containers:")
	app.UI.Card(ui.Card{
		Title:    "Fluxer runs from " + moved.Dir,
		Body:     body,
		Commands: []string{"cd " + shellWord(moved.Dir), res.tool + " ps", "rexctl migrate --clean-up --dir " + shellWord(old.Dir)},
	})
}

func undoHandOver(ctx context.Context, app *App, moved, old *instance.Instance, confirm string) error {
	backups := moved.Path(instance.BackupsDir)
	what := fmt.Sprintf("This stops Fluxer in %s, backs it up into %s and starts the old containers in %s again, with the data from before the move. Anything posted after the move is gone from the old one. rexctl keeps the volume of the new instance. Any change rexctl made to Valkey or SeaweedFS before the move stays.", moved.Dir, backups, old.Dir)
	if err := app.UI.ConfirmDestructive(what, moved.Domain(), confirm); err != nil {
		return err
	}
	if err := os.Chdir(old.Dir); err != nil {
		return ui.Wrap(err, "Couldn't open "+old.Dir+". rexctl didn't go back.", "Check that "+old.Dir+" is there and readable, then run "+migrateCommand+" --undo again.")
	}
	if err := oldImagesHere(ctx, app, old); err != nil {
		return err
	}
	r, err := app.Runtime(ctx, moved)
	if err != nil {
		return err
	}
	back := schedules{backups: composedSchedule(moved, old)}
	var undone rt.Undone
	err = app.Locked(ctx, moved, func() error {
		return app.UI.Step(ctx, "Stopping and backing up Fluxer in "+moved.Dir, func(ctx context.Context, s *ui.Step) error {
			var err error
			undone, err = rt.UndoHandOver(ctx, r, s, backups, old.Dir)
			if errors.Is(err, rt.ErrNotHandedOver) {
				return ui.Refused(fmt.Sprintf("rexctl can't go back from %s, because compose.yaml there no longer runs %s on the volume %s.", moved.Dir, rt.ImageRepo, rt.VolumeName(moved)), "Put back the compose.yaml rexctl wrote there, then run "+migrateCommand+" --undo again.")
			}
			if err != nil {
				return rt.ContainerError(err, "Couldn't stop and back up Fluxer in "+moved.Dir+". rexctl didn't go back.")
			}
			s.Done("Stopped Fluxer in %s and backed it up to %s.", moved.Dir, undone.Backup)
			return nil
		})
	})
	if err != nil {
		return err
	}
	rules := moved.State.MovedFrom.Firewall
	if err := keepRules(moved, rules); err != nil {
		app.UI.Detail("%v", err)
	}
	stepAway(app, moved)
	old.State.UndoneVolume = undone.Volume
	if err := finishUndo(ctx, app, old, rules, back); err != nil {
		return err
	}
	if line := rt.ForeignLine(undone.Foreign); line != "" {
		app.UI.Info("%s", line)
	}
	if len(undone.Renamed) > 0 {
		app.UI.Info("Renamed compose.yaml and fluxer.env in %s so they don't start Fluxer again.", moved.Dir)
	}
	engineTool := string(composeEngine(moved))
	app.UI.Info("rexctl kept the volume %s with the data from after the move, and a backup of it is in %s. Remove the volume with %s volume rm %s once you're sure you don't need it.", undone.Volume, undone.Backup, engineTool, undone.Volume)
	if moved.State.MovedFrom.AutoUpgrade {
		app.UI.Info("Automatic upgrades from before the move didn't come back. Turn them on again with %s.", sudoRun(old.Dir, "auto-upgrade", "on"))
	}
	return nil
}

func composedSchedule(moved, old *instance.Instance) string {
	if fluxerdSchedule(old) != host.ScheduleOff {
		return ""
	}
	f, err := envfile.Load(moved.Path(rt.EnvFileName))
	if err != nil {
		return ""
	}
	switch v := strings.ToLower(strings.TrimSpace(f.Value(fluxerdScheduleVar))); v {
	case host.ScheduleDaily, host.ScheduleWeekly:
		return v
	}
	return ""
}

func undoneVolumeLeft(ctx context.Context, app *App, inst *instance.Instance) error {
	name := inst.State.UndoneVolume
	if name == "" {
		return nil
	}
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return nil
	}
	if ok, err := e.VolumeExists(ctx, name); err != nil || !ok {
		return nil
	}
	return ui.Refused(fmt.Sprintf("The volume %s is left from a move you undid.", name), fmt.Sprintf("Check that you don't need it, remove it with %s volume rm %s, then run this again.", composeEngine(inst), name))
}
