// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"net"
	"os"
	"os/exec"
	"os/user"
	"regexp"
	goruntime "runtime"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type nativeRT struct {
	inst     *instance.Instance
	u        *ui.UI
	n        *native.Stack
	dataLeft bool
	ctr      *voiceCtr
}

func newNative(inst *instance.Instance, u *ui.UI) *nativeRT {
	return &nativeRT{inst: inst, u: u, n: native.New(inst)}
}

func (r *nativeRT) root() error {
	if r.ctr != nil {
		return nil
	}
	if goruntime.GOOS != "linux" || !host.BootUnitSupported() {
		return unsupported("A native instance runs only on Linux with systemd.", "Run rexctl on the Linux machine Fluxer runs on.")
	}
	if os.Geteuid() != 0 {
		return needsRoot("A native instance is managed as root.")
	}
	return nil
}

func NativeAlone(ctx context.Context) error {
	if host.StandaloneNativeInUse(ctx) {
		return NativeError(host.ErrStandaloneNative, "")
	}
	return nil
}

type stepLog struct{ s *ui.Step }

func (w stepLog) Write(p []byte) (int, error) {
	if w.s != nil {
		for _, line := range strings.Split(strings.TrimRight(string(p), "\n"), "\n") {
			w.s.Log(line)
		}
	}
	return len(p), nil
}

func progressOf(s *ui.Step) func(cur, total int64) {
	return func(cur, total int64) {
		if s != nil {
			s.Progress(cur, total)
		}
	}
}

func (r *nativeRT) Kind() Kind { return Native }

func (r *nativeRT) live(ctx context.Context) bool {
	switch r.n.ActiveState(ctx) {
	case "active", "activating", "reloading":
		return true
	}
	return false
}

func (r *nativeRT) Info(ctx context.Context) Info {
	return Info{Kind: Native, Label: NativeLabel, Unit: r.n.Unit(), Root: true, DataDir: r.n.Root(), EdgeBind: r.n.EdgeBind()}
}

func (r *nativeRT) Status(ctx context.Context) (Status, error) {
	st := Status{Release: r.inst.State.Release, RunningRelease: r.n.Running()}
	if r.inst.Busy() {
		st.Busy = "Another rexctl command is changing this instance."
	}
	ns := r.n.Status(ctx)
	st.Running = ns.Unit == "active" || ns.Unit == "activating" || ns.Unit == "reloading"
	st.Restarts = ns.Restarts
	st.Unit = ns.Unit
	for _, c := range ns.Containers {
		if c.State == "running" && (st.StartedAt.IsZero() || c.Created.Before(st.StartedAt)) {
			st.StartedAt = c.Created
		}
	}
	st.Parts = withEmbedded(foldParts(ns.Containers), ns.Containers, st.StartedAt, r.inst.State.Settings)
	return st, nil
}

func (r *nativeRT) apply(ctx context.Context, mode native.ApplyMode) (native.Applied, error) {
	set, err := deploy.ReleaseSet(r.inst)
	if err != nil {
		return native.Applied{}, err
	}
	a, err := ApplyNative(ctx, r.inst, r.n, set, r.u.Warn, mode)
	if err != nil || r.inst.State.Pending == nil {
		return a, err
	}
	r.inst.State.Pending = nil
	return a, saveState(r.inst)
}

func ApplyNative(ctx context.Context, inst *instance.Instance, n *native.Stack, set *stack.Set, warn func(msg, fix string), mode native.ApplyMode) (native.Applied, error) {
	wasStopped := inst.State.Stopped
	if err := setStopped(n, false); err != nil {
		return native.Applied{}, err
	}
	inst.State.Stopped = false
	a, err := n.Apply(ctx, set, warn, mode)
	if err != nil {
		if wasStopped {
			inst.State.Stopped = true
			_ = setStopped(n, true)
		}
		return a, NativeError(err, "Couldn't set up Fluxer's service.")
	}
	if inst.State.BootService == instance.BootServiceOff && n.Enabled(ctx) {
		if err := n.Disable(ctx); err != nil {
			return a, NativeError(err, "Couldn't keep Fluxer from starting with this machine.")
		}
	}
	if err := inst.Save(); err != nil {
		return a, ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return a, nil
}

func (r *nativeRT) wait(ctx context.Context, s *ui.Step, verb string) error {
	return NativeWait(ctx, r.n, s, verb)
}

func NativeWait(ctx context.Context, n *native.Stack, s *ui.Step, verb string) error {
	return NativeReadyError(n, n.WaitReady(ctx, engine.DefaultReadyTimeout, ShowReadiness(s, verb+" Fluxer")))
}

func NativeReadyError(n *native.Stack, err error) error {
	if err == nil || errors.Is(err, context.Canceled) {
		return err
	}
	fix := fmt.Sprintf("Run rexctl logs, or journalctl -u %s, to see why.", n.Unit())
	var nr *engine.NotReadyError
	if errors.As(err, &nr) {
		msg := "Fluxer didn't become ready in time."
		switch {
		case nr.Fatal || slices.Contains(nr.Crashing, native.Supervisor):
			msg = "Fluxer's service keeps stopping."
		case len(nr.Crashing) == 1:
			msg = PartOf(nr.Crashing[0]) + " keeps stopping."
		case len(nr.Crashing) > 1:
			msg = ui.JoinAnd(partsOf(nr.Crashing)) + " keep stopping."
		}
		return ui.Wrap(err, msg, fix)
	}
	return ui.Wrap(err, "Fluxer didn't start.", fix)
}

func partsOf(services []string) []string {
	var out []string
	for _, s := range services {
		if p := PartOf(s); !slices.Contains(out, p) {
			out = append(out, p)
		}
	}
	return out
}

func setStopped(n *native.Stack, stopped bool) error {
	path, dir := n.StoppedMarker(), n.Inst.Dir
	if !stopped {
		if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
			return ui.Wrap(err, "Couldn't clear the mark that keeps Fluxer stopped.", "Check that root can write to "+dir+", then try again.")
		}
		return nil
	}
	if err := os.WriteFile(path, nil, 0o600); err != nil {
		return ui.Wrap(err, "Couldn't mark Fluxer as stopped.", "Check that root can write to "+dir+", then try again.")
	}
	return nil
}

func (r *nativeRT) Start(ctx context.Context, s *ui.Step, o StartOptions) error {
	if err := r.root(); err != nil {
		return err
	}
	if err := NativeAlone(ctx); err != nil {
		return err
	}
	wasStopped := r.inst.State.Stopped
	a, err := r.apply(ctx, native.ApplyChanges)
	if err != nil {
		return err
	}
	if !a.Started && !a.Restarted {
		if err := r.n.StartStopped(ctx); err != nil {
			return r.stayStopped(ctx, wasStopped, NativeError(err, "Couldn't start Fluxer's stopped services."))
		}
	}
	if o.NoWait {
		return nil
	}
	return r.stayStopped(ctx, wasStopped, r.wait(ctx, s, "Starting"))
}

func (r *nativeRT) stayStopped(ctx context.Context, wasStopped bool, err error) error {
	if err == nil || !wasStopped {
		return err
	}
	ctx = context.WithoutCancel(ctx)
	r.inst.State.Stopped = true
	if serr := setStopped(r.n, true); serr == nil {
		_ = r.keepStopped(ctx)
	}
	_ = r.n.Stop(ctx)
	_ = r.inst.Save()
	return err
}

func (r *nativeRT) Stop(ctx context.Context, s *ui.Step) error {
	if err := r.root(); err != nil {
		return err
	}
	if err := setStopped(r.n, true); err != nil {
		return err
	}
	if err := r.keepStopped(ctx); err != nil {
		_ = setStopped(r.n, false)
		return NativeError(err, "Couldn't keep Fluxer stopped when this machine restarts.")
	}
	if err := r.n.Stop(context.WithoutCancel(ctx)); err != nil {
		_ = setStopped(r.n, false)
		return NativeError(err, "Couldn't stop Fluxer.")
	}
	return nil
}

func (r *nativeRT) keepStopped(ctx context.Context) error {
	u := r.n.BootUnit()
	if u.KeepsStopped() {
		return nil
	}
	steps, err := u.RefreshSteps(ctx)
	if err == nil {
		err = host.RunSteps(ctx, steps)
	}
	if err == nil && u.KeepsStopped() {
		return nil
	}
	if err != nil {
		r.u.Detail("%v", err)
	}
	if !r.n.Enabled(ctx) {
		return nil
	}
	return r.n.Disable(ctx)
}

func (r *nativeRT) Restart(ctx context.Context, s *ui.Step, parts []string) error {
	var children []string
	if len(parts) > 0 {
		var err error
		if children, err = r.children(parts); err != nil {
			return err
		}
	}
	if err := r.root(); err != nil {
		return err
	}
	if err := NativeAlone(ctx); err != nil {
		return err
	}
	if len(children) > 0 {
		if entries, err := r.n.Entries(ctx); err == nil {
			children = slices.DeleteFunc(children, func(c string) bool {
				return !slices.ContainsFunc(entries, func(e native.PsEntry) bool { return e.Name == c })
			})
		}
		if len(children) == 0 {
			return unsupported("This instance runs without "+ui.JoinAnd(parts)+".", "Run rexctl status to see what runs.")
		}
		if err := r.n.Restart(ctx, children...); err != nil {
			return NativeError(err, "Couldn't restart "+ui.JoinAnd(parts)+".")
		}
		return r.wait(ctx, s, "Restarting")
	}
	if _, err := r.apply(ctx, native.ApplyRestart); err != nil {
		return err
	}
	return r.wait(ctx, s, "Restarting")
}

func (r *nativeRT) Down(ctx context.Context, s *ui.Step) error {
	if err := r.Stop(ctx, s); err != nil {
		return err
	}
	if err := os.RemoveAll(r.n.DefaultRuntimeDir()); err != nil {
		r.u.Detail("%v", err)
	}
	return nil
}

func readsJournal() bool {
	if os.Geteuid() == 0 {
		return true
	}
	g, err := user.LookupGroup("systemd-journal")
	if err != nil {
		return false
	}
	gid, err := strconv.Atoi(g.Gid)
	if err != nil {
		return false
	}
	groups, _ := os.Getgroups()
	return slices.Contains(groups, gid) || os.Getegid() == gid
}

func (r *nativeRT) Logs(ctx context.Context, w io.Writer, o LogOptions) error {
	lp, err := planLogs(r.inst, o.Parts)
	if err != nil {
		return err
	}
	if !readsJournal() {
		return needsRoot("Only root and the systemd-journal group can read Fluxer's logs.")
	}
	args := native.LogsArgs(r.n.Unit(), engine.LogsOptions{Follow: o.Follow, Since: o.Since, Tail: o.Tail}, lp.services...)
	pr, pw := io.Pipe()
	done := make(chan error, 1)
	go func() {
		err := journalLines(pr, w, lp)
		pr.CloseWithError(err)
		done <- err
	}()
	err = native.Journal(ctx, append(args, "-o", "json"), pw)
	pw.Close()
	if lerr := <-done; err == nil {
		err = lerr
	}
	if err != nil && ctx.Err() == nil {
		return ui.Wrap(err, "Couldn't read the logs.", "Run rexctl logs as root, or as an account in the systemd-journal group.")
	}
	return nil
}

type journalEntry struct {
	Identifier string          `json:"SYSLOG_IDENTIFIER"`
	Message    json.RawMessage `json:"MESSAGE"`
}

func journalMessage(raw json.RawMessage) string {
	var s string
	if json.Unmarshal(raw, &s) == nil {
		return s
	}
	var b []byte
	var nums []int
	if json.Unmarshal(raw, &nums) == nil {
		for _, n := range nums {
			b = append(b, byte(n))
		}
	}
	return string(b)
}

var natsLogLine = regexp.MustCompile(`^(\[\d+\] )?(\d{4}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}\.\d+ )?\[(INF|WRN|ERR|DBG|TRC|FTL)\] `)

func journalService(id, msg string) string {
	if id == native.Supervisor {
		if natsLogLine.MatchString(msg) {
			return "nats"
		}
		return "edge"
	}
	if id == "fluxer-seaweedfs-filer" {
		return "seaweedfs"
	}
	return strings.TrimPrefix(id, "fluxer-")
}

func journalLines(r io.Reader, w io.Writer, lp logPlan) error {
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 64*1024), 4*1024*1024)
	for sc.Scan() {
		var e journalEntry
		if json.Unmarshal(sc.Bytes(), &e) != nil {
			continue
		}
		msg := strings.TrimRight(journalMessage(e.Message), "\n")
		service := journalService(e.Identifier, msg)
		if e.Identifier == native.Supervisor && len(lp.services) > 0 && !slices.Contains(lp.services, service) {
			continue
		}
		out, keep := lp.line(service, msg)
		if !keep {
			continue
		}
		if _, err := io.WriteString(w, out+"\n"); err != nil {
			return err
		}
	}
	return sc.Err()
}

func (r *nativeRT) plan(o UninstallOptions) Removal {
	var rm Removal
	if path, err := r.n.BootUnit().Path(); err == nil {
		rm.Goes = append(rm.Goes, ui.KV{Key: "Service", Value: path})
	}
	rm.Goes = append(rm.Goes, ui.KV{Key: "Releases", Value: sized(r.n.ReleasesDir())})
	settingsAt := r.inst.Dir
	if r.n.SettingsDir() != r.n.Root() {
		settingsAt = r.n.EnvFile()
	}
	if o.KeepData {
		rm.Stays = append(rm.Stays, ui.KV{Key: "Data", Value: sized(r.n.Root())}, ui.KV{Key: "Settings", Value: settingsAt})
		rm.Kept = "Your data is still in " + r.n.Root() + " and your settings are still in " + settingsAt + "."
	} else {
		rm.Goes = append(rm.Goes, ui.KV{Key: "Data", Value: sized(r.n.Root())})
		if settingsAt != r.inst.Dir {
			rm.Goes = append(rm.Goes, ui.KV{Key: "Settings", Value: settingsAt})
		}
		rm.Goes, rm.Stays = folderRows(r.inst, o, rm.Goes, rm.Stays)
	}
	rm.Goes, rm.Stays = backupRows(r.inst, o, rm.Goes, rm.Stays)
	rm.Goes = append(rm.Goes, firewallRows(r.inst)...)
	rm.Stays = append(rm.Stays, proxyRows()...)
	if packages, err := r.n.Packages(r.inst.State.Release); err == nil && len(packages) > 0 {
		for _, m := range r.n.Postgres().Keep() {
			if p := native.PackageName(m); !slices.Contains(packages, p) {
				packages = append(packages, p)
			}
		}
		rm.Stays = append(rm.Stays, ui.KV{Key: "Packages", Value: strings.Join(packages, ", ")})
	}
	if native.NoMainCluster() {
		rm.Stays = append(rm.Stays, ui.KV{Key: "PostgreSQL", Value: "create_main_cluster = false in " + native.CreateClusterConf + ", so apt makes no default cluster"})
	}
	if _, err := user.Lookup(native.ServiceUser); err == nil && !otherNative(r.inst) {
		rm.Account = native.ServiceUser
	}
	return rm
}

func otherNative(inst *instance.Instance) bool {
	for _, o := range instance.Others(inst.Dir) {
		if o.State.IsNative() {
			return true
		}
	}
	return false
}

func removeAccount(ctx context.Context, name string) error {
	out, err := exec.CommandContext(ctx, "userdel", name).CombinedOutput()
	if err != nil {
		return fmt.Errorf("userdel %s: %w: %s", name, err, strings.TrimSpace(string(out)))
	}
	return nil
}

func (r *nativeRT) Uninstall(ctx context.Context, s *ui.Step, o UninstallOptions) error {
	if err := r.root(); err != nil {
		return err
	}
	ctx = context.WithoutCancel(ctx)
	var left []string
	if err := r.n.Stop(ctx); err != nil {
		switch r.n.ActiveState(ctx) {
		case "inactive", "failed", "":
			r.u.Detail("%v", err)
		default:
			return &ui.ExitError{Code: ui.ExitFailed, Msg: "Couldn't stop Fluxer, so rexctl removed nothing.", Fix: "Run rexctl stop, then run rexctl uninstall again.", Err: err}
		}
	}
	u := r.n.BootUnit()
	steps, err := u.RemoveSteps(ctx)
	if err == nil {
		err = host.RunSteps(ctx, append(steps, host.LibexecRemoveSteps(ctx, u)...))
	}
	if err != nil {
		return NativeError(err, "Couldn't remove Fluxer's service.")
	}
	if err := os.RemoveAll(r.n.DefaultRuntimeDir()); err != nil {
		r.u.Detail("%v", err)
	}
	if err := os.RemoveAll(r.n.ReleasesDir()); err != nil {
		r.u.Detail("%v", err)
		left = append(left, r.n.ReleasesDir()+" is still on this machine.")
	}
	for _, link := range []string{r.n.Current(), r.inst.Path(native.PreviousLink)} {
		_ = os.Remove(link)
	}
	if !o.KeepData {
		if err := r.n.RemoveData(); err != nil {
			r.u.Detail("%v", err)
			r.dataLeft = true
			left = append(left, "Your data is still in "+r.n.Root()+".")
		}
		if err := r.n.RemoveSettings(); err != nil {
			r.u.Detail("%v", err)
			left = append(left, "Your settings are still in "+r.n.SettingsDir()+".")
		}
	}
	if o.RemoveUser {
		if err := removeAccount(ctx, native.ServiceUser); err != nil {
			r.u.Detail("%v", err)
			left = append(left, "The account "+native.ServiceUser+" is still on this machine.")
		}
	}
	stillThere(s, append(left, closeFirewall(ctx, r.inst)...))
	return nil
}

func (r *nativeRT) Boot() Boot { return nativeBoot{r} }

func dialOperator(socket string) error {
	c, err := net.DialTimeout("unix", socket, 2*time.Second)
	if err != nil {
		return err
	}
	return c.Close()
}

func (r *nativeRT) Admin(ctx context.Context) (adminapi.Transport, error) {
	socket := r.n.OperatorSocket()
	if os.Geteuid() != 0 {
		if err := dialOperator(socket); errors.Is(err, fs.ErrPermission) {
			return nil, needsRoot("Only root can talk to Fluxer on a native instance.")
		}
	}
	return adminapi.NewHost(socket, r.n.APIAddr(), deploy.ClientIPHeader(r.inst)), nil
}

func (r *nativeRT) Settings() Settings { return nativeSettings{r} }
func (r *nativeRT) Backups() Backups   { return nativeBackups{r} }
func (r *nativeRT) Upgrades() Upgrades { return nativeUpgrades{r} }

type nativeBoot struct{ r *nativeRT }

func (b nativeBoot) State(ctx context.Context) (BootState, error) {
	if goruntime.GOOS != "linux" || !host.BootUnitSupported() {
		return BootState{}, b.r.root()
	}
	path, err := b.r.n.BootUnit().Path()
	if err != nil {
		return BootState{}, err
	}
	return BootState{Enabled: b.r.n.Enabled(ctx), Unit: path, Detail: "Fluxer keeps running until you stop it or this machine restarts."}, nil
}

func (b nativeBoot) steps(ctx context.Context, enable bool) ([]host.Step, error) {
	st, err := b.State(ctx)
	switch {
	case err != nil:
		return nil, err
	case !exists(st.Unit) && enable:
		return nil, ui.Failed("rexctl couldn't find the service that starts Fluxer.", "Run rexctl start to set it up again.")
	case !exists(st.Unit) || st.Enabled == enable:
		return nil, nil
	}
	unit := b.r.n.Unit()
	if enable {
		return []host.Step{{Do: "Starts " + unit + " with the machine from now on.", Command: "systemctl enable " + unit, Run: b.r.n.Enable}}, nil
	}
	return []host.Step{{Do: "Stops starting " + unit + " with the machine. Fluxer keeps running.", Command: "systemctl disable " + unit, Run: b.r.n.Disable}}, nil
}

func (b nativeBoot) Enable(ctx context.Context, s *ui.Step) error {
	if err := b.r.root(); err != nil {
		return err
	}
	return NativeError(b.r.n.Enable(ctx), "Couldn't make Fluxer start with this machine.")
}

func (b nativeBoot) Disable(ctx context.Context, s *ui.Step) error {
	if err := b.r.root(); err != nil {
		return err
	}
	return NativeError(b.r.n.Disable(ctx), "Couldn't stop Fluxer starting with this machine.")
}
