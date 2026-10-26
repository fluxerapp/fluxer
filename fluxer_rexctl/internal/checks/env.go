// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"maps"
	"math"
	"net"
	"os"
	"os/exec"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var ErrNoFix = errors.New("checks: this check has no fix")

const defaultBind = "127.0.0.1:8080"

type edgeMode int

const (
	modeBundled edgeMode = iota
	modeProxy
	modeTunnel
)

func (e *Env) prepare(ctx context.Context) {
	if e.prepared {
		return
	}
	e.prepared = true
	if e.Instance != nil {
		if e.Dir == "" {
			e.Dir = e.Instance.Dir
		}
		if e.Domain == "" {
			e.Domain = e.value("FLUXER_DOMAIN")
		}
		if e.Bind == "" {
			e.Bind = e.value("FLUXER_EDGE_BIND")
		}
	}
	if e.Bind == "" {
		e.Bind = defaultBind
	}
	if e.Engine == nil && e.engineErr == nil {
		opts := engine.ConnectOptions{AllowOld: true}
		if st := e.state(); st != nil {
			opts.Host, opts.ID, opts.Prefer = st.Engine.Host, st.Engine.ID, engine.Kind(st.Engine.Kind)
		}
		e.Engine, e.engineErr = engine.Connect(ctx, opts)
	}
	if e.Project == nil && e.Engine != nil && e.Instance != nil && e.Instance.Project() != "" {
		p, err := deploy.Load(ctx, e.Instance, e.Engine)
		if err != nil {
			e.loadErr = err
			p = e.Engine.Named(e.Instance.Project(), e.Instance.Dir)
		}
		e.Project = p
	}
	if e.Host == nil {
		if info, err := host.Detect(ctx, e.Dir); err == nil {
			e.Host = info
		}
	}
}

func (e *Env) SetEngine(en *engine.Engine, err error) {
	e.Engine, e.engineErr = en, err
}

func (e *Env) reset() {
	e.ps, e.psErr, e.psDone = nil, nil, false
	e.pub, e.pubErr, e.pubDone = nil, nil, false
}

func (e *Env) published(ctx context.Context) (map[host.Port]string, bool) {
	if e.pubDone {
		return e.pub, e.pubErr == nil
	}
	e.pubDone = true
	if e.Engine == nil {
		e.pubErr = errNoProject
		return nil, false
	}
	owners, err := e.Engine.PublishedHostPorts(ctx)
	if err != nil {
		e.pubErr = err
		return nil, false
	}
	e.pub = map[host.Port]string{}
	for _, o := range owners {
		key := host.Port{Number: o.Number, Proto: o.Proto}
		if _, ok := e.pub[key]; !ok {
			e.pub[key] = o.Project
		}
	}
	return e.pub, true
}

func (e *Env) voicePorts() (tcp, udp int) {
	tcp, udp = 7881, 7882
	if n, err := strconv.Atoi(e.value(stack.VoiceTCPPortKey)); err == nil && n > 0 && n <= 65535 {
		tcp = n
	}
	if n, err := strconv.Atoi(e.value(stack.VoiceUDPPortKey)); err == nil && n > 0 && n <= 65535 {
		udp = n
	}
	return tcp, udp
}

func (e *Env) state() *instance.State {
	if e.Instance == nil {
		return nil
	}
	return e.Instance.State
}

func (e *Env) effective() map[string]string {
	if e.env != nil {
		return e.env
	}
	e.env = map[string]string{}
	st := e.state()
	if st == nil {
		return e.env
	}
	set, err := e.Instance.Set()
	if err != nil {
		return e.env
	}
	chosen := maps.Clone(st.Settings)
	for k, v := range e.staged {
		if v == nil {
			delete(chosen, k)
		} else {
			chosen[k] = *v
		}
	}
	secrets := maps.Clone(st.Secrets)
	maps.Copy(secrets, e.stagedSecrets)
	admin := map[string]string{}
	if st.AdminImport != nil {
		maps.Copy(admin, st.AdminImport.Values)
	}
	maps.Copy(admin, e.stagedAdmin)
	if env, err := settings.Effective(set, chosen, secrets, admin); err == nil {
		e.env = env
	}
	return e.env
}

func (e *Env) setting(key string) string {
	if v, ok := e.staged[key]; ok {
		if v == nil {
			return ""
		}
		return *v
	}
	if st := e.state(); st != nil {
		return st.Settings[key]
	}
	return ""
}

func (e *Env) value(key string) string {
	v, _ := e.lookup(key)
	return strings.TrimSpace(v)
}

func (e *Env) lookup(key string) (string, bool) {
	if v, ok := e.Settings[key]; ok {
		return v, true
	}
	v, ok := e.effective()[key]
	return v, ok
}

func (e *Env) stage(key string, value *string) {
	if e.staged == nil {
		e.staged = map[string]*string{}
	}
	e.staged[key] = value
	e.env = nil
}

func (e *Env) stageSecret(key, value string) {
	if e.stagedSecrets == nil {
		e.stagedSecrets = map[string]string{}
	}
	e.stagedSecrets[key] = value
	e.env = nil
}

func (e *Env) stageAdmin(key, value string) {
	if e.stagedAdmin == nil {
		e.stagedAdmin = map[string]string{}
	}
	e.stagedAdmin[key] = value
	e.env = nil
}

func (e *Env) stagedCount() int {
	n := len(e.staged) + len(e.stagedSecrets) + len(e.stagedAdmin)
	if e.importEnv {
		n++
	}
	return n
}

func (e *Env) Staged() bool { return e.stagedCount() > 0 }

func (e *Env) StagedBy() []string { return e.stagedBy }

func Commit(ctx context.Context, e *Env, o deploy.ApplyOptions) error {
	if !e.Staged() {
		return nil
	}
	if e.Instance == nil || e.Instance.State == nil || e.Engine == nil {
		return errNoProject
	}
	st := e.Instance.State
	staged, secrets, admin, importEnv := e.staged, e.stagedSecrets, e.stagedAdmin, e.importEnv
	e.staged, e.stagedSecrets, e.stagedAdmin, e.importEnv, e.stagedBy = nil, nil, nil, false, nil
	e.env = nil
	envPath := e.Instance.Path(instance.EnvFileName)
	aside := e.Instance.Path(instance.StateDirName, "env.import")
	if importEnv {
		if err := os.Rename(envPath, aside); err != nil && !errors.Is(err, os.ErrNotExist) {
			return err
		}
	}
	done := false
	defer func() {
		if !importEnv {
			return
		}
		if done {
			_ = os.Remove(aside)
		} else {
			_ = os.Rename(aside, envPath)
		}
	}()
	if len(secrets) > 0 || len(admin) > 0 {
		maps.Copy(st.Secrets, secrets)
		if len(admin) > 0 {
			if st.AdminImport == nil {
				st.AdminImport = &instance.AdminImport{}
			}
			if st.AdminImport.Values == nil {
				st.AdminImport.Values = map[string]string{}
			}
			maps.Copy(st.AdminImport.Values, admin)
			st.AdminImport.SentAt = ""
		}
		if err := e.Instance.Save(); err != nil {
			return err
		}
	}
	var err error
	if len(staged) > 0 {
		_, err = deploy.Set(ctx, e.Instance, e.Engine, staged, o)
	} else {
		_, err = deploy.Apply(ctx, e.Instance, e.Engine, o)
	}
	if err != nil {
		return err
	}
	done = true
	e.Project = nil
	e.prepared = false
	e.prepare(ctx)
	e.reset()
	return nil
}

func (e *Env) ports() []stack.Port {
	return stack.Ports(map[string]string{
		stack.VoiceTCPPortKey: e.value(stack.VoiceTCPPortKey),
		stack.VoiceUDPPortKey: e.value(stack.VoiceUDPPortKey),
	})
}

func (e *Env) mode() edgeMode {
	if e.state() != nil {
		switch e.setting(settings.EdgeMode) {
		case settings.EdgeTunnel:
			return modeTunnel
		case settings.EdgeProxy:
			return modeProxy
		}
	}
	if e.Behind {
		return modeProxy
	}
	return modeBundled
}

func (e *Env) installed() bool { return e.phase&Installed != 0 }

func (e *Env) podmanSocketOff() bool {
	if runtime.GOOS != "linux" || e.Engine != nil || engine.ClassOf(e.engineErr) != engine.ClassUnreachable {
		return false
	}
	if _, err := exec.LookPath("podman"); err != nil {
		return false
	}
	if st := e.state(); st != nil && st.Engine.Kind != "" {
		return st.Engine.Kind == string(engine.Podman)
	}
	_, err := exec.LookPath("docker")
	return err != nil
}

func (e *Env) rootless() bool {
	if e.Engine != nil {
		return e.Engine.Rootless
	}
	return e.podmanSocketOff() && os.Geteuid() != 0
}

func (e *Env) podman() bool {
	return (e.Engine != nil && e.Engine.Kind == engine.Podman) || e.podmanSocketOff()
}

func (e *Env) reconnect(ctx context.Context) {
	e.Engine, e.engineErr, e.Project, e.loadErr = nil, nil, nil, nil
	e.prepared = false
	e.prepare(ctx)
}

func (e *Env) engineName() string {
	if e.podman() {
		return "Podman"
	}
	if e.Engine == nil && e.state() != nil && e.state().Engine.Kind == string(engine.Podman) {
		return "Podman"
	}
	return "Docker"
}

func (e *Env) containers(ctx context.Context) ([]engine.Container, error) {
	if e.psDone {
		return e.ps, e.psErr
	}
	e.psDone = true
	if e.Project == nil {
		e.psErr = errNoProject
		return nil, e.psErr
	}
	e.ps, e.psErr = e.Project.PS(ctx)
	return e.ps, e.psErr
}

var errNoProject = errors.New("checks: no instance or container engine")

func (e *Env) running(ctx context.Context, service string) bool {
	cs, err := e.containers(ctx)
	if err != nil {
		return false
	}
	for _, c := range cs {
		if c.Service == service && c.State == "running" {
			return true
		}
	}
	return false
}

func (e *Env) exec(ctx context.Context, service, stdin string, command ...string) (string, error) {
	if e.Project == nil {
		return "", errNoProject
	}
	ctx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	var out, errOut bytes.Buffer
	opts := engine.ExecOptions{Stdout: &out, Stderr: &errOut}
	if stdin != "" {
		opts.Stdin = strings.NewReader(stdin)
	}
	err := e.Project.Exec(ctx, service, opts, command...)
	combined := strings.TrimSpace(out.String() + "\n" + errOut.String())
	return combined, err
}

func (e *Env) publicAddress(ctx context.Context) (string, error) {
	if !e.publicDone {
		e.publicDone = true
		e.publicIP, e.publicErr = host.PublicIP(ctx)
	}
	return e.publicIP, e.publicErr
}

func privateDomain(domain string) bool {
	d := strings.ToLower(strings.TrimSuffix(domain, "."))
	if d == "" || d == "localhost" || !strings.Contains(d, ".") {
		return true
	}
	if net.ParseIP(d) != nil {
		return true
	}
	for _, suffix := range []string{".localhost", ".local", ".test", ".internal", ".lan", ".home.arpa", ".example", ".invalid"} {
		if strings.HasSuffix(d, suffix) {
			return true
		}
	}
	return false
}

func gb(bytes uint64) string {
	return fmt.Sprintf("%d GB", int(math.Round(float64(bytes)/(1<<30))))
}

func day(t time.Time) string { return t.Local().Format("2 January") }

func Services(names []string, one, many string) string {
	switch {
	case len(names) == 1:
		return names[0] + " " + one
	case len(names) <= 3:
		return strings.Join(names[:len(names)-1], ", ") + " and " + names[len(names)-1] + " " + many
	}
	return fmt.Sprintf("%d services %s", len(names), many)
}

func pass(msg string) Result { return Result{Status: Pass, Message: msg} }

func skip(msg string) Result { return Result{Status: Skip, Message: msg} }

func warn(msg, fix string) Result { return Result{Status: Warn, Message: msg, Fix: fix} }

func fail(msg, fix string) Result { return Result{Status: Fail, Message: msg, Fix: fix} }

func fixable(r Result) Result {
	r.Fixable = true
	return r
}

func (e *Env) up(ctx context.Context) error {
	if e.Instance == nil || e.Engine == nil {
		return errNoProject
	}
	if e.loadErr != nil {
		return e.loadErr
	}
	_, err := deploy.Apply(ctx, e.Instance, e.Engine, deploy.ApplyOptions{})
	return err
}
