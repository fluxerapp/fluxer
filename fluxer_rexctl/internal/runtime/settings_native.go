// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"maps"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const checkConfigTimeout = 30 * time.Second

type nativeSettings struct{ r *nativeRT }

func (nativeSettings) Supports(key string) (bool, string) {
	if settings.DockerOnly(key) {
		return false, key + " only changes Docker containers, and this instance runs natively without them."
	}
	if why, ok := native.Unsupported(key); ok {
		return false, key + " has no effect on this instance. " + why
	}
	return true, ""
}

func (n nativeSettings) Validate(ctx context.Context, p *deploy.Prepared) error {
	if p == nil || p.Unchanged {
		return nil
	}
	if err := n.r.root(); err != nil {
		return err
	}
	var names, reasons []string
	for _, k := range p.Keys {
		if ok, why := n.Supports(k); !ok {
			names, reasons = append(names, k), append(reasons, why)
		}
	}
	switch len(names) {
	case 0:
	case 1:
		return unsupported(reasons[0], "Leave it out.")
	default:
		return unsupported(ui.JoinAnd(names)+" have no effect on this instance, because it runs natively.", "Leave them out.")
	}
	if err := bindProblem(p.Settings); err != nil {
		return err
	}
	if err := deploy.CheckHostPorts(ctx, n.r.inst, p); err != nil {
		return err
	}
	return n.checkConfig(ctx, p)
}

func CanChangeSettings(r Runtime) error {
	if n, ok := r.(*nativeRT); ok {
		return n.root()
	}
	return nil
}

func bindProblem(next map[string]string) error {
	mode := next[settings.EdgeMode]
	if mode == "" || mode == settings.EdgeBundled {
		return nil
	}
	bind := next[settings.EdgeBind]
	if _, err := native.ProxyListen(bind); err != nil {
		fix := "Run rexctl proxy on --bind " + settings.DefaultBind + "."
		if mode == settings.EdgeTunnel {
			fix = "Run rexctl config set edge.bind " + settings.DefaultBind + "."
		}
		return &ui.ExitError{Code: ui.ExitRefused, Err: err,
			Msg: fmt.Sprintf("A native instance can't listen on %s behind your proxy.", bind), Fix: fix}
	}
	return nil
}

func (n nativeSettings) candidate(p *deploy.Prepared) (env, secrets map[string]string, err error) {
	st := n.r.inst.State
	oldSettings, oldSecrets := st.Settings, st.Secrets
	st.Settings, st.Secrets = p.Settings, p.Secrets
	defer func() { st.Settings, st.Secrets = oldSettings, oldSecrets }()
	return n.r.n.Environment(p.Set)
}

func (n nativeSettings) checkConfig(ctx context.Context, p *deploy.Prepared) error {
	out, err := n.runCheck(ctx, p, false)
	var ee *exec.ExitError
	switch {
	case err == nil || errors.Is(err, errNoCheck):
		return nil
	case !errors.As(err, &ee):
		return err
	}
	text := strings.TrimSpace(out)
	if strings.Contains(text, "flag provided but not defined") {
		return nil
	}
	return &ui.ExitError{Code: ui.ExitRefused, Err: err, Tail: strings.Split(text, "\n"),
		Msg: "Fluxer can't start with that change, so rexctl left your settings as they were.",
		Fix: "Fix what fluxerd reports below, then try again."}
}

var errNoCheck = errors.New("runtime: fluxerd can't check the settings here")

func (n nativeSettings) runCheck(ctx context.Context, p *deploy.Prepared, against bool) (string, error) {
	fluxerd := n.r.n.Fluxerd()
	if os.Geteuid() != 0 {
		return "", errNoCheck
	}
	if _, err := os.Stat(fluxerd); err != nil {
		return "", errNoCheck
	}
	inst := n.r.inst
	dir := inst.Path(instance.StateDirName)
	env, secrets, err := n.candidate(p)
	if err != nil {
		return "", NativeError(err, "rexctl can't work out Fluxer's settings with that change.")
	}
	maps.Copy(env, secrets)
	path, err := writeCheckEnv(dir, env)
	if path != "" {
		defer os.Remove(path)
	}
	if err != nil {
		return "", ui.Wrap(err, "rexctl couldn't write the settings for fluxerd to check, so it left your settings as they were.",
			"Check that "+inst.Dir+" has free space and that you can write to it, then try again.")
	}
	args := []string{"check-config", "--config", path, "--caddy-dir", inst.Path(instance.CaddyDropInDir), "--certs-dir", inst.Path(instance.CertsDir)}
	if against {
		env, secrets, err := n.r.n.Environment(p.Set)
		if err != nil {
			return "", NativeError(err, "rexctl can't work out Fluxer's settings.")
		}
		maps.Copy(env, secrets)
		cur, err := writeCheckEnv(dir, env)
		if cur != "" {
			defer os.Remove(cur)
		}
		if err != nil {
			return "", err
		}
		args = append(args, "--against", cur)
	}
	cctx, cancel := context.WithTimeout(ctx, checkConfigTimeout)
	defer cancel()
	cmd := exec.CommandContext(cctx, fluxerd, args...)
	cmd.Env = []string{"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin", "HOME=" + n.r.n.Root(),
		"FLUXER_DATA_DIR=" + n.r.n.Root(), "RUNTIME_DIRECTORY=" + n.r.n.RuntimeDir()}
	out, err := cmd.CombinedOutput()
	switch {
	case ctx.Err() != nil:
		return "", ctx.Err()
	case errors.Is(cctx.Err(), context.DeadlineExceeded):
		return "", &ui.ExitError{Code: ui.ExitFailed, Err: cctx.Err(),
			Msg: fmt.Sprintf("fluxerd didn't finish checking the new settings within %d seconds, so rexctl left your settings as they were.", int(checkConfigTimeout/time.Second)),
			Fix: "Run rexctl doctor to see what's slowing it down, then try again."}
	}
	return string(out), err
}

func reloadParts(out string) ([]string, bool) {
	for _, line := range strings.Split(out, "\n") {
		rest, ok := strings.CutPrefix(strings.TrimSpace(line), "reload: ")
		if !ok {
			continue
		}
		var restart, edge bool
		var kids []string
		for _, f := range strings.Fields(rest) {
			k, v, _ := strings.Cut(f, "=")
			switch k {
			case "restart":
				restart = v == "true"
			case "edge":
				edge = v == "true"
			case "children":
				if v != "" {
					kids = strings.Split(v, ",")
				}
			}
		}
		if restart {
			return []string{"all"}, true
		}
		parts := partsOf(kids)
		if edge && !slices.Contains(parts, "edge") {
			parts = append(parts, "edge")
		}
		return parts, true
	}
	return nil, false
}

func writeCheckEnv(dir string, env map[string]string) (string, error) {
	f, err := os.CreateTemp(dir, ".check-*.env")
	if err != nil {
		return "", err
	}
	var b bytes.Buffer
	for _, k := range slices.Sorted(maps.Keys(env)) {
		b.WriteString(k + "=" + envfile.Quote(env[k]) + "\n")
	}
	_, werr := f.Write(b.Bytes())
	return f.Name(), errors.Join(werr, f.Close())
}

func (n nativeSettings) Preview(ctx context.Context, p *deploy.Prepared) (Preview, error) {
	if p == nil || p.Unchanged {
		return Preview{}, nil
	}
	pv := Preview{Notes: p.Preview.Notes()}
	restarts, err := n.restarts(p)
	if err != nil || !restarts {
		return pv, err
	}
	pv.Restarts = []string{"all"}
	if out, err := n.runCheck(ctx, p, true); err == nil {
		if parts, ok := reloadParts(out); ok {
			pv.Restarts = parts
		}
	}
	return pv, nil
}

func (n nativeSettings) restarts(p *deploy.Prepared) (bool, error) {
	env, secrets, err := n.r.n.Environment(p.Set)
	if err != nil {
		return false, NativeError(err, "rexctl can't work out Fluxer's settings.")
	}
	nextEnv, nextSecrets, err := n.candidate(p)
	if err != nil {
		return false, NativeError(err, "rexctl can't work out Fluxer's settings with that change.")
	}
	return !maps.Equal(env, nextEnv) || !maps.Equal(secrets, nextSecrets), nil
}

func (n nativeSettings) Apply(ctx context.Context, s *ui.Step, p *deploy.Prepared) (*deploy.Change, error) {
	r := n.r
	st := r.inst.State
	if p.Unchanged {
		return &deploy.Change{Keys: p.Keys, Unchanged: true}, nil
	}
	if err := r.root(); err != nil {
		return nil, err
	}
	if st.Upgrade != nil || st.Stopped || !r.live(ctx) {
		st.Settings, st.Secrets = p.Settings, p.Secrets
		return &deploy.Change{Keys: p.Preview.Keys}, saveState(r.inst)
	}
	if err := NativeAlone(ctx); err != nil {
		return nil, err
	}
	apply := func(ctx context.Context, title string) ([]string, error) {
		return r.applySettings(ctx, s, p.Set, title)
	}
	logs := func(ctx context.Context, w io.Writer) error {
		return r.n.Logs(ctx, engine.LogsOptions{Tail: startLogLines, Stdout: w})
	}
	return applyPending(ctx, r.inst, p, apply, logs)
}

type applyFunc func(ctx context.Context, title string) (restarted []string, err error)

func applyPending(ctx context.Context, inst *instance.Instance, p *deploy.Prepared, apply applyFunc, logs func(context.Context, io.Writer) error) (*deploy.Change, error) {
	st := inst.State
	changed := &deploy.Change{Keys: p.Preview.Keys}
	oldSettings, oldSecrets := st.Settings, st.Secrets
	st.Pending = &instance.Pending{Settings: p.Settings, Keys: p.Preview.Keys, StartedAt: time.Now().UTC()}
	if err := saveState(inst); err != nil {
		st.Pending = nil
		return nil, err
	}
	st.Settings, st.Secrets = p.Settings, p.Secrets
	restarted, err := apply(ctx, "Applying the new settings")
	if err == nil {
		st.Pending = nil
		changed.Applied = true
		changed.Restarted = restarted
		if serr := saveState(inst); serr != nil {
			if serr = saveState(inst); serr != nil {
				return changed, ui.Wrap(serr, "Fluxer runs with the new settings, but rexctl couldn't save them. Fluxer goes back to the old ones the next time rexctl starts it or changes a setting.",
					"Check that you can write to "+inst.Dir+", then run the same command again.")
			}
		}
		return changed, nil
	}
	logPath := writeStartLog(inst, err, func(w io.Writer) error {
		return logs(context.WithoutCancel(ctx), w)
	})
	st.Settings, st.Secrets = oldSettings, oldSecrets
	_, rerr := apply(context.WithoutCancel(ctx), "Putting the old settings back")
	st.Pending = nil
	if serr := saveState(inst); serr != nil && rerr == nil {
		rerr = serr
	}
	changed.Reverted = rerr == nil
	return changed, ApplyFailure(changed, errors.Join(err, rerr), logPath)
}

func resolvePending(ctx context.Context, inst *instance.Instance, apply applyFunc) error {
	st := inst.State
	if st.Pending == nil {
		return nil
	}
	if !st.Stopped {
		if _, err := apply(context.WithoutCancel(ctx), "Putting the old settings back"); err != nil {
			return err
		}
	}
	st.Pending = nil
	return saveState(inst)
}

func saveState(inst *instance.Instance) error {
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, "Couldn't save your instance's state.", "Check that you can write to "+inst.Dir+", then try again.")
	}
	return nil
}

func (r *nativeRT) applySettings(ctx context.Context, s *ui.Step, set *stack.Set, title string) ([]string, error) {
	a, err := ApplyNative(ctx, r.inst, r.n, set, r.u.Warn, native.ApplyChanges)
	if err != nil {
		return nil, err
	}
	restarted := partsOf(a.Reloaded)
	if a.Started || a.Restarted {
		return []string{"all"}, r.waitTitled(ctx, s, title)
	}
	if len(restarted) == 0 || slices.Equal(restarted, []string{"edge"}) {
		s = nil
	}
	return restarted, r.waitTitled(ctx, s, title)
}

func (r *nativeRT) waitTitled(ctx context.Context, s *ui.Step, title string) error {
	return NativeReadyError(r.n, r.n.WaitReady(ctx, engine.DefaultReadyTimeout, ShowReadiness(s, title)))
}

func (n nativeSettings) Render(ctx context.Context, w io.Writer) error {
	set, err := deploy.ReleaseSet(n.r.inst)
	if err != nil {
		return err
	}
	env, secrets, err := n.r.n.Environment(set)
	if err != nil {
		return NativeError(err, "rexctl can't work out Fluxer's settings.")
	}
	var b bytes.Buffer
	for _, k := range slices.Sorted(maps.Keys(env)) {
		if settings.DockerOnly(k) {
			continue
		}
		v := settings.Mask(settings.Key{Name: k}, env[k])
		if settings.SecretName(k) && env[k] != "" {
			v = settings.Masked
		}
		b.WriteString(k + "=" + envfile.Quote(v) + "\n")
	}
	for _, k := range slices.Sorted(maps.Keys(secrets)) {
		if _, shown := env[k]; !shown {
			b.WriteString(k + "=" + settings.Masked + "\n")
		}
	}
	_, err = w.Write(b.Bytes())
	return err
}

func ResolvePending(ctx context.Context, rt Runtime, s *ui.Step) error {
	switch r := rt.(type) {
	case *compose:
		e, err := r.engine(ctx)
		if err != nil {
			return err
		}
		return r.engineError(deploy.Resolve(ctx, r.inst, e), "Couldn't put the old settings back.")
	case *containerRT:
		return resolveContainerPending(ctx, r, s)
	case *nativeRT:
		if r.inst.State.Pending == nil {
			return nil
		}
		if err := r.root(); err != nil {
			return err
		}
		return resolvePending(ctx, r.inst, func(ctx context.Context, title string) ([]string, error) {
			set, err := deploy.ReleaseSet(r.inst)
			if err != nil {
				return nil, err
			}
			return r.applySettings(ctx, s, set, title)
		})
	}
	return nil
}

const startLogLines = 200

func writeStartLog(inst *instance.Instance, cause error, logs func(io.Writer) error) string {
	dir := inst.Path(instance.StateDirName)
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return ""
	}
	path := filepath.Join(dir, "start.log")
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return ""
	}
	defer f.Close()
	fmt.Fprintf(f, "rexctl: %v\n\n", cause)
	_ = logs(f)
	return path
}
