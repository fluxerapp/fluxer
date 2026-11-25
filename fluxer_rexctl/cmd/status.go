// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const statusEvery = 2 * time.Second

type statusPart struct {
	Name     string   `json:"name"`
	State    string   `json:"state"`
	Health   string   `json:"health"`
	Members  []string `json:"members"`
	Restarts int      `json:"restarts"`
	Optional bool     `json:"optional"`
}

type statusHealth struct {
	Status  string `json:"status"`
	Message string `json:"message"`
	Fix     string `json:"fix,omitempty"`
}

type statusBackup struct {
	Last     *time.Time `json:"last"`
	Count    int        `json:"count"`
	Bytes    int64      `json:"bytes"`
	Schedule string     `json:"schedule"`
}

type statusDisk struct {
	FreeBytes *int64 `json:"free_bytes"`
}

type statusNewer struct {
	Version string `json:"version"`
	command string
}

type statusJSON struct {
	URL            string       `json:"url"`
	Domain         string       `json:"domain"`
	Release        string       `json:"release"`
	RunningRelease string       `json:"running_release"`
	Runtime        string       `json:"runtime"`
	Engine         string       `json:"engine"`
	Project        string       `json:"project"`
	Dir            string       `json:"dir"`
	Running        bool         `json:"running"`
	Healthy        bool         `json:"healthy"`
	Stopped        bool         `json:"stopped"`
	Busy           bool         `json:"busy"`
	Uptime         int64        `json:"uptime"`
	Restarts       int          `json:"restarts"`
	Boot           *bool        `json:"boot"`
	Health         statusHealth `json:"health"`
	Backup         statusBackup `json:"backup"`
	Disk           statusDisk   `json:"disk"`
	Upgrade        string       `json:"upgrade"`
	Layout         string       `json:"layout,omitempty"`
	LastRelease    string       `json:"last_release,omitempty"`
	Newer          *statusNewer `json:"newer"`
	Parts          []statusPart `json:"parts"`
}

type statusModel struct {
	out      statusJSON
	info     rt.Info
	parts    []rt.Part
	err      error
	hidden   bool
	starting bool
	pgFailed bool
	headline ui.Tone
	msg, fix string
	exit     int
}

func newStatusCmd(app *App) *cobra.Command {
	var watch bool
	c := &cobra.Command{
		Use:   "status",
		Short: "Show whether your instance is running",
		Long:  "Show whether your instance is running, with each part of Fluxer, the address, version, folder, whether it starts with this machine and when it was last backed up. With --watch, the card stays open and updates every few seconds.",
		Example: "rexctl status\n" +
			"rexctl status --watch\n" +
			"rexctl status --json",
		Args: noArgs,
		RunE: func(c *cobra.Command, _ []string) error {
			return runStatus(c.Context(), app, watch)
		},
	}
	c.Flags().BoolVar(&watch, "watch", false, "keep the card open and update it every few seconds")
	app.jsonFlag(c)
	return c
}

func runStatus(ctx context.Context, app *App, watch bool) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	if inst.State.IsVoice() {
		if app.Flags.JSON || watch {
			return listVoiceMachine(ctx, app, inst)
		}
		return voiceMachineStatus(ctx, app, inst)
	}
	if moved := inst.Moved(); moved != nil {
		return movedRefusal(inst, moved)
	}
	r, err := app.Runtime(ctx, inst)
	if err != nil {
		return err
	}
	if watch && !app.Flags.JSON {
		return watchStatus(ctx, app, inst, r)
	}
	m := readStatus(ctx, inst, r)
	if m.starting && !app.Flags.JSON {
		m = waitStarted(ctx, app, m, startWait, func(ctx context.Context) statusModel { return readStatus(ctx, inst, r) })
	}
	if m.exit != 0 {
		app.SetExit(m.exit)
	}
	if app.Flags.JSON {
		return app.UI.JSON(m.out)
	}
	showStatus(ctx, app, inst, r, m)
	return nil
}

func readStatus(ctx context.Context, inst *instance.Instance, r rt.Runtime) statusModel {
	st, err := r.Status(ctx)
	m := statusModel{err: err, parts: st.Parts, info: r.Info(ctx)}
	m.out = statusJSON{
		URL:            inst.Origin(),
		Domain:         inst.Domain(),
		Release:        inst.State.Release,
		RunningRelease: st.RunningRelease,
		Runtime:        string(r.Kind()),
		Engine:         inst.State.Engine.Kind,
		Project:        inst.Project(),
		Dir:            inst.Dir,
		Running:        st.Running,
		Stopped:        inst.State.Stopped,
		Busy:           inst.Busy(),
		Parts:          []statusPart{},
	}
	if u := inst.State.Upgrade; u != nil {
		m.out.Upgrade = u.To
	}
	if legacyCompose(inst) {
		m.out.Layout = "legacy"
		m.out.LastRelease = inst.State.Release
		if m.out.Upgrade == "" {
			m.out.Upgrade = "migrate"
		}
	}
	if b, err := r.Boot().State(ctx); err == nil {
		m.out.Boot = &b.Enabled
	}
	if list, err := backup.List(inst.BackupDir()); err == nil && len(list) > 0 {
		t := list[0].CreatedAt
		m.out.Backup.Last = &t
		m.out.Backup.Count = len(list)
		for _, b := range list {
			m.out.Backup.Bytes += b.SizeBytes
		}
	}
	m.out.Backup.Schedule = backupScheduleOf(r, inst)
	if dir := m.info.DataDir; dir != "" {
		if free, err := backup.FreeSpace(dir); err == nil {
			n := int64(free)
			m.out.Disk.FreeBytes = &n
		}
	}
	m.out.Restarts = st.Restarts
	if st.Running {
		m.out.Newer = newerRelease(ctx, r, instance.UpgradeCommand(inst.Dir))
	}
	if st.Running && !st.StartedAt.IsZero() {
		m.out.Uptime = int64(time.Since(st.StartedAt) / time.Second)
	}
	var failed, stopped, waiting []string
	for _, p := range st.Parts {
		m.out.Parts = append(m.out.Parts, statusPart{Name: p.Name, State: string(p.State), Health: p.Health, Members: p.Members, Restarts: p.Restarts, Optional: p.Optional})
		if p.Optional {
			continue
		}
		switch p.State {
		case rt.PartFailed:
			failed = append(failed, p.Name)
		case rt.PartStopped:
			stopped = append(stopped, p.Name)
		case rt.PartStarting, rt.PartDegraded:
			waiting = append(waiting, p.Name)
		}
	}
	m.hidden = err == nil && st.Running && len(st.Parts) == 0
	m.out.Healthy = err == nil && st.Running && !m.hidden && len(failed)+len(stopped)+len(waiting) == 0
	start := "Run rexctl start."
	var ej *instance.EjectedError
	if errors.As(inst.Managed(), &ej) {
		start = ej.Start()
	}
	busy := m.out.Busy && !m.out.Healthy
	from, finish := unrecordedMove(inst)
	restoreMsg, restoreFix, restoring := inst.RestoreUnfinished()
	var ee *ui.ExitError
	switch {
	case errors.As(err, &ee):
		m.headline, m.msg, m.fix = ui.ToneFail, ee.Msg, ee.Fix
	case err != nil:
		m.headline, m.msg, m.fix = ui.ToneFail, ui.Sentence(err.Error()), "Run rexctl doctor to see why."
	case busy && inst.State.Upgrade != nil:
		m.headline, m.msg, m.fix = ui.ToneWarn, fmt.Sprintf("Fluxer is being upgraded to %s.", inst.State.Upgrade.To), "Run rexctl status again when the upgrade is done."
	case busy:
		m.headline, m.msg, m.fix = ui.ToneWarn, "Another rexctl command is changing this instance.", "Run rexctl status again when it's done."
	case finish != "" && moveUnderWay(from):
		m.headline, m.msg, m.fix = ui.ToneWarn, fmt.Sprintf("Fluxer is moving here from %s.", from), "Run rexctl status again when the move is done."
	case finish != "" && !st.Running:
		m.headline, m.msg, m.fix = ui.ToneFail, fmt.Sprintf("The move from %s stopped before rexctl recorded it.", from), fmt.Sprintf("Run %s first, then %s.", finish, startHere(inst))
	case finish != "":
		m.headline, m.msg, m.fix = ui.ToneWarn, fmt.Sprintf("Fluxer runs here, but the move from %s isn't recorded yet.", from), fmt.Sprintf("Run %s to record it.", finish)
	case restoring && ej == nil:
		m.headline, m.msg, m.fix = ui.ToneFail, restoreMsg, restoreFix
	case !st.Running && ej == nil && inst.State.InstallUnfinished():
		m.headline, m.msg, m.fix = ui.ToneFail, "The install didn't finish.", "Run "+installAgain(r)+" again to finish it."
	case !st.Running && inst.State.Stopped:
		m.headline, m.msg, m.fix = ui.ToneFail, "Fluxer is stopped.", start
	case !st.Running:
		m.headline, m.msg, m.fix = ui.ToneFail, "Fluxer isn't running.", strings.TrimSuffix(start, ".")+", or rexctl logs to see why it stopped."
	case m.hidden && crashLooping(st):
		m.headline, m.msg, m.fix = ui.ToneFail, "Fluxer's service keeps stopping.", "Run rexctl logs to see why."
	case m.hidden && os.Geteuid() != 0 && m.info.Root:
		m.headline, m.msg, m.fix = ui.ToneWarn, "Fluxer is running, and only root can see its parts.", "Run "+sudoAgain(inst.Dir)+" to see them."
	case m.hidden:
		m.headline, m.msg, m.fix = ui.ToneWarn, "Fluxer is starting.", "Run rexctl status again in a minute."
	case len(failed) > 0:
		m.headline, m.msg, m.fix = ui.ToneFail, partsLine(failed, "keeps failing.", "keep failing."), "Run rexctl doctor to find out why."
	case len(stopped) > 0:
		m.headline, m.msg, m.fix = ui.ToneFail, partsLine(stopped, "isn't running.", "aren't running."), start
	case len(waiting) > 0:
		m.starting = justStarted(st, time.Now())
		m.headline, m.msg, m.fix = ui.ToneWarn, partsLine(waiting, "isn't ready yet.", "aren't ready yet."), "Run rexctl doctor if this doesn't clear up in a few minutes."
	case checks.EdgeStarting(ctx, statusEnv(inst, r)):
		m.out.Healthy, m.starting = false, true
		m.headline, m.msg, m.fix = ui.ToneWarn, "Fluxer is starting.", "Run rexctl status again in a minute."
	default:
		m.headline, m.msg = ui.ToneOK, "Fluxer is running."
	}
	if err == nil && !busy && !m.out.Healthy && !restoring {
		if c, ok := checks.Get("postgres-version"); ok {
			if res := checks.RunIn(ctx, statusEnv(inst, r), checks.Installed, c); res.Status == checks.Fail {
				m.headline, m.msg, m.fix, m.pgFailed = ui.ToneFail, res.Message, res.Fix, true
			}
		}
	}
	if m.headline == ui.ToneFail || err != nil {
		m.exit = ui.ExitRefused
	}
	m.out.Health = statusHealth{Status: toneWord(m.headline), Message: m.msg, Fix: m.fix}
	return m
}

const startWait = time.Minute

func crashLooping(st rt.Status) bool {
	return st.Running && len(st.Parts) == 0 && native.CrashLooping(st.Unit, st.Restarts)
}

func justStarted(st rt.Status, now time.Time) bool {
	return st.Running && (st.StartedAt.IsZero() || now.Sub(st.StartedAt) < startWait)
}

func waitStarted(ctx context.Context, app *App, m statusModel, wait time.Duration, read func(context.Context) statusModel) statusModel {
	_ = app.UI.Step(ctx, "Waiting for Fluxer to start", func(ctx context.Context, s *ui.Step) error {
		deadline := time.Now().Add(wait)
		for m.starting && time.Now().Before(deadline) {
			select {
			case <-ctx.Done():
				return nil
			case <-time.After(min(statusEvery, wait)):
			}
			m = read(ctx)
		}
		if m.starting {
			s.Warned("still starting")
		} else {
			s.Done("started")
		}
		return nil
	})
	return m
}

func unrecordedMove(inst *instance.Instance) (string, string) {
	from := inst.State.MovedFrom
	if from == nil {
		return "", ""
	}
	st, err := migrate.LoadState(from.Dir)
	if err != nil || st.Docker == nil || st.Docker.SwitchedAt == nil || instance.RealPath(st.Docker.Dir) != instance.RealPath(inst.Dir) {
		return "", ""
	}
	return from.Dir, finishMove(from.Dir, inst)
}

func finishMove(oldDir string, moved *instance.Instance) string {
	to := cmpDir(moved.State.Engine.Kind, string(engine.Docker))
	if rt.KindOf(moved) == rt.Native {
		to = runtimeSystemd
	}
	if old, err := instance.Find(oldDir); euid() != 0 && (to == runtimeSystemd || err != nil || rootOwned(old)) {
		return sudoRun(oldDir, "migrate", "--to", to)
	}
	return "rexctl migrate --to " + to + " --dir " + shellWord(oldDir)
}

func moveUnderWay(dir string) bool {
	old, err := instance.Find(dir)
	return err == nil && old.Busy()
}

func startHere(inst *instance.Instance) string {
	if euid() != 0 && rootOwned(inst) {
		return sudoRun(inst.Dir, "start")
	}
	return "rexctl start"
}

func newerRelease(ctx context.Context, r rt.Runtime, command string) *statusNewer {
	stack := rt.FluxerdStack(r)
	if stack == nil {
		return nil
	}
	u, err := stack.Update(ctx)
	switch {
	case err != nil:
		return nil
	case u.Outdated:
		return &statusNewer{command: command}
	case len(u.Newer) == 0:
		return nil
	}
	return &statusNewer{Version: u.Newer[0].Version, command: command}
}

func newerRow(n *statusNewer) ui.KV {
	switch {
	case n.Version == "":
		return ui.KV{Key: "New release", Value: "A newer Fluxer is out. Run " + n.command + ".", State: ui.StateActive}
	}
	return ui.KV{Key: "New release", Value: "Fluxer " + n.Version + " is out. Run " + n.command + ".", State: ui.StateActive}
}

func partsLine(names []string, one, many string) string {
	if len(names) == 1 {
		return "Fluxer's " + names[0] + " part " + one
	}
	return "Fluxer's " + ui.JoinAnd(names) + " parts " + many
}

func toneWord(t ui.Tone) string {
	switch t {
	case ui.ToneOK:
		return "ok"
	case ui.ToneWarn:
		return "warn"
	}
	return "fail"
}

func moodOf(m statusModel) ui.Mood {
	switch {
	case m.out.Busy && !m.out.Healthy:
		return ui.MoodThink
	case m.out.Healthy:
		return ui.MoodIdle
	case !m.out.Running && m.out.Stopped:
		return ui.MoodSleep
	case m.headline == ui.ToneWarn:
		return ui.MoodWorried
	}
	return ui.MoodSad
}

func installAgain(r rt.Runtime) string {
	switch r.Kind() {
	case rt.Native:
		return "rexctl install --runtime systemd"
	case rt.Container:
		if strings.HasPrefix(r.Info(context.Background()).Label, "Podman") {
			return "rexctl install --runtime podman"
		}
		return "rexctl install --runtime docker"
	}
	return "rexctl install"
}

func showStatus(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime, m statusModel) {
	app.UI.StatusCard(statusView(m, time.Now()))
	if m.out.Running {
		rootCertCard(app.UI, saveRootCert(ctx, app, inst, 0), inst.Origin())
	}
	if m.fix != "" {
		app.UI.Info("%s", m.fix)
	}
	var ej *instance.EjectedError
	if errors.As(inst.Managed(), &ej) {
		app.UI.Info("%s %s", ej.Error(), ej.Fix())
	}
	if u := inst.State.Upgrade; u != nil && ej == nil && !inst.Busy() {
		app.UI.Warn(fmt.Sprintf("The upgrade to %s didn't finish.", u.To), release.UnfinishedFix(u))
	}
	if m.out.Healthy && !inst.State.SetupDone {
		warnSetup(ctx, app, inst, r)
	}
	if m.err == nil {
		env := statusEnv(inst, r)
		for _, id := range []string{"disk", "objects-filesystem", "uploads-read-only", "certificate", "postgres-version", "move-download"} {
			if c, ok := checks.Get(id); ok && !(m.pgFailed && id == "postgres-version") {
				checkCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
				warnCheck(app, checks.RunIn(checkCtx, env, checks.Installed, c))
				cancel()
			}
		}
	}
	warnUnusedRootFiles(ctx, app, inst)
	if legacyCompose(inst) {
		legacyStatusLines(app)
	}
	cleanUpLine(app, inst)
	if len(instance.UnseenEdits(inst)) > 0 {
		app.UI.Warn(fmt.Sprintf("rexctl put back its own version of files you changed. Your copies are in %s.", inst.Path(instance.StateDirName, instance.EditedDir)), "Run rexctl doctor to see them.")
	}
}

func warnSetup(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime) {
	client, err := app.runtimeAdmin(ctx, inst, r)
	if err != nil {
		return
	}
	if done, err := setup.Done(ctx, client); err != nil || done {
		return
	}
	fix := "Run rexctl setup to finish it here, or rexctl setup token to finish it in your browser."
	if exists, err := setup.AccountsExist(ctx, client); err == nil && exists {
		fix = browserSetupFix(inst.Origin())
	}
	app.UI.Warn("Setup isn't finished yet.", fix)
}

func scheduleShort(every string) string {
	switch every {
	case host.ScheduleDaily:
		return "every night"
	case host.ScheduleWeekly:
		return "once a week"
	}
	return ""
}

func statusView(m statusModel, now time.Time) ui.View {
	v := ui.View{Title: m.out.Domain, Headline: m.msg, Tone: m.headline, Mood: moodOf(m), Updated: now}
	meta := []ui.KV{{Key: "Address", Value: m.out.URL}}
	if version := m.out.Release; version != "" {
		if m.out.RunningRelease != "" && m.out.RunningRelease != version {
			version += ", running " + m.out.RunningRelease
		}
		if n := m.out.Newer; n != nil && n.Version != "" {
			version += ", " + n.Version + " is out"
		}
		meta = append(meta, ui.KV{Key: "Version", Value: version})
	}
	meta = append(meta, ui.KV{Key: "Runs on", Value: m.info.Label}, ui.KV{Key: "Folder", Value: m.out.Dir})
	v.Meta = meta
	backupRow := ui.KV{Key: "Backup", Value: "None yet", State: ui.StateWarn, Hint: "Run rexctl backup to make one."}
	b := m.out.Backup
	switch every := scheduleShort(b.Schedule); {
	case b.Last != nil:
		backupRow = ui.KV{Key: "Backup", Value: fmt.Sprintf("%s, %d kept, %s", ago(now.Sub(*b.Last)), b.Count, ui.Bytes(b.Bytes))}
		if every != "" {
			backupRow.Value += ", " + every
		}
	case every != "":
		backupRow = ui.KV{Key: "Backup", Value: "None yet, the first one runs " + every}
	}
	v.Data = append(v.Data, backupRow)
	if m.out.Disk.FreeBytes != nil {
		v.Data = append(v.Data, ui.KV{Key: "Disk", Value: ui.Bytes(*m.out.Disk.FreeBytes) + " free"})
	}
	if m.out.Boot != nil {
		boot := "Starts with this machine"
		if !*m.out.Boot {
			boot = "Doesn't start with this machine"
		}
		v.Data = append(v.Data, ui.KV{Key: "Boot", Value: boot})
	}
	if m.out.Newer != nil && m.out.Upgrade == "" {
		v.Data = append(v.Data, newerRow(m.out.Newer))
	}
	if m.out.Upgrade != "" && !(m.out.Layout == "legacy" && m.out.Upgrade == "migrate") {
		v.Data = append(v.Data, ui.KV{Key: "Upgrade", Value: "to " + m.out.Upgrade + " didn't finish", State: ui.StateWarn})
	}
	for _, p := range m.parts {
		row := ui.PartRow{Name: p.Name, Members: strings.Join(p.Members, ", "), State: partState(p.State), Restarts: strconv.Itoa(p.Restarts)}
		if !p.Since.IsZero() && p.State != rt.PartStopped && p.State != rt.PartFailed {
			row.Uptime = span(now.Sub(p.Since))
		}
		if p.CPU >= 0 {
			row.CPU = fmt.Sprintf("%.0f%%", p.CPU)
		}
		if p.MemBytes >= 0 {
			row.Memory = ui.Bytes(p.MemBytes)
		}
		v.Parts = append(v.Parts, row)
	}
	return v
}

func partState(s rt.PartState) ui.State {
	switch s {
	case rt.PartRunning:
		return ui.StateOK
	case rt.PartStarting:
		return ui.StateActive
	case rt.PartDegraded:
		return ui.StateWarn
	case rt.PartFailed:
		return ui.StateFail
	case rt.PartAbsent:
		return ui.StateSkipped
	}
	return ui.StatePending
}

func ago(d time.Duration) string {
	switch {
	case d < time.Minute:
		return "just now"
	case d < time.Hour:
		return plural(int(d/time.Minute), "minute", "minutes") + " ago"
	case d < 48*time.Hour:
		return plural(int(d/time.Hour), "hour", "hours") + " ago"
	}
	return plural(int(d/(24*time.Hour)), "day", "days") + " ago"
}

func span(d time.Duration) string {
	switch {
	case d < time.Minute:
		return "<1m"
	case d < time.Hour:
		return fmt.Sprintf("%dm", int(d/time.Minute))
	case d < 24*time.Hour:
		return fmt.Sprintf("%dh %dm", int(d/time.Hour), int(d%time.Hour/time.Minute))
	}
	return fmt.Sprintf("%dd %dh", int(d/(24*time.Hour)), int(d%(24*time.Hour)/time.Hour))
}

func watchStatus(ctx context.Context, app *App, inst *instance.Instance, r rt.Runtime) error {
	fetch := func(ctx context.Context) (ui.View, error) {
		return statusView(readStatus(ctx, inst, r), time.Now()), nil
	}
	actions := []ui.Action{
		{Key: "l", Help: "logs", Run: func(ctx context.Context, part string) error {
			o := rt.LogOptions{Tail: 200, Follow: true}
			if part != "" {
				o.Parts = []string{part}
			}
			return r.Logs(ctx, os.Stdout, o)
		}},
		{Key: "r", Help: "restart part", Confirm: "Restart this part of Fluxer?", Run: func(ctx context.Context, part string) error {
			var parts []string
			if part != "" {
				parts = []string{part}
			}
			return app.Locked(ctx, inst, func() error { return r.Restart(ctx, nil, parts) })
		}},
		{Key: "b", Help: "backup", Run: func(ctx context.Context, _ string) error { return runBackup(ctx, app, "manual") }},
		{Key: "d", Help: "doctor", Run: func(ctx context.Context, _ string) error { return runDoctor(ctx, app, false, false) }},
		{Key: "u", Help: "upgrade", Run: func(ctx context.Context, _ string) error { return runUpgrade(ctx, app, &upgradeFlags{}) }},
	}
	return app.UI.Watch(ctx, fetch, statusEvery, actions...)
}

func warnUnusedRootFiles(ctx context.Context, app *App, inst *instance.Instance) {
	if app.Flags.JSON {
		return
	}
	for _, name := range inst.UnusedRootFiles(ctx) {
		app.UI.Warn(fmt.Sprintf("Fluxer no longer uses %s, which you changed after rexctl moved its files. Put your Caddy directives in a .caddy file in %s/ instead.", inst.Path(name), instance.CaddyDropInDir), "")
	}
}

func statusEnv(inst *instance.Instance, r rt.Runtime) *checks.Env {
	return &checks.Env{Dir: inst.Dir, Instance: inst, Native: r.Kind() != rt.Compose, Container: r.Kind() == rt.Container, Stack: rt.FluxerdStack(r), Downloads: leftDownloads(inst, time.Now())}
}

func warnCheck(app *App, r checks.Result) {
	if r.Status == checks.Warn || r.Status == checks.Fail {
		app.UI.Warn(r.Message, r.Fix)
	}
}
