// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"maps"
	"net/netip"
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
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var errNoFix = errors.New("checks: this check has no fix")

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
	e.fillAddress()
	if e.Engine == nil && e.engineErr == nil && !e.Native {
		opts := engine.ConnectOptions{AllowOld: true}
		if st := e.state(); st != nil {
			opts.Host, opts.ID, opts.Prefer = st.Engine.Host, st.Engine.ID, engine.Kind(st.Engine.Kind)
		}
		e.Engine, e.engineErr = engine.Connect(ctx, opts)
	}
	if e.Project == nil && e.Engine != nil && e.Instance != nil && e.Instance.Project() != "" && !e.Instance.State.IsVoice() {
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

func (e *Env) fillAddress() {
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
		e.Bind = settings.DefaultBind
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
	e.stageSetting(key, value, false)
}

func (e *Env) stageSetting(key string, value *string, secret bool) {
	if e.staged == nil {
		e.staged = map[string]*string{}
	}
	if e.dryRun {
		e.stageStep(key, e.settingStep(key, value, secret))
	}
	e.staged[key] = value
	e.env = nil
}

func (e *Env) settingStep(key string, value *string, secret bool) host.Step {
	set := func(v string) string {
		if secret {
			return "rexctl config set " + key + " -"
		}
		return "rexctl config set " + key + " " + instance.ShellQuote(v)
	}
	undo := "rexctl config unset " + key
	if old := e.setting(key); old != "" {
		undo = set(old)
	}
	if value == nil {
		return host.Step{Do: "Removes " + key + " from your settings.", Command: "rexctl config unset " + key, Undo: undo}
	}
	do := "Sets " + key + " to " + *value + "."
	if secret {
		do = "Sets " + key + " to the value in .env."
	}
	return host.Step{Do: do, Command: set(*value), Undo: undo}
}

func (e *Env) stageSecret(key, value string) {
	if e.stagedSecrets == nil {
		e.stagedSecrets = map[string]string{}
	}
	e.stageStep("secret "+key, host.Step{Do: "Saves " + key + " in the secrets rexctl keeps."})
	e.stagedSecrets[key] = value
	e.env = nil
}

func (e *Env) stageAdmin(key, value string) {
	if e.stagedAdmin == nil {
		e.stagedAdmin = map[string]string{}
	}
	e.stageStep("admin "+key, host.Step{Do: "Sends " + key + " from .env to your instance's admin settings."})
	e.stagedAdmin[key] = value
	e.env = nil
}

func (e *Env) stageEnvImport(unused []string) {
	do := "Deletes .env from " + e.Instance.Dir + " once the settings are applied."
	if len(unused) > 0 {
		do += " Fluxer doesn't use " + ui.JoinAnd(unused) + "."
	}
	e.stageStep(".env", host.Step{Do: do})
	e.importEnv = true
}

func (e *Env) stageStep(key string, step host.Step) {
	if !e.dryRun {
		return
	}
	if e.stageSteps == nil {
		e.stageSteps = map[string]int{}
	}
	if i, ok := e.stageSteps[key]; ok {
		step.Undo = e.steps[i].Undo
		e.steps[i] = step
		return
	}
	e.stageSteps[key] = len(e.steps)
	e.steps = append(e.steps, step)
}

func (e *Env) run(ctx context.Context, steps ...host.Step) error {
	if e.dryRun {
		e.steps = append(e.steps, steps...)
		return nil
	}
	return host.RunSteps(ctx, steps)
}

func (e *Env) reload(ctx context.Context) {
	e.Project, e.loadErr = nil, nil
	e.prepared = false
	e.prepare(ctx)
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
	if e.Instance == nil || e.Instance.State == nil || (e.Engine == nil && e.ApplySettings == nil) {
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
	switch {
	case e.Engine == nil:
		err = e.ApplySettings(ctx, staged)
	case len(staged) > 0:
		_, err = deploy.Set(ctx, e.Instance, e.Engine, staged, o)
	default:
		_, err = deploy.Apply(ctx, e.Instance, e.Engine, o)
	}
	if err != nil {
		return err
	}
	done = true
	e.reload(ctx)
	e.reset()
	return nil
}

func (e *Env) ports() []stack.Port {
	return stack.Ports(map[string]string{
		stack.VoiceTCPPortKey: e.value(stack.VoiceTCPPortKey),
		stack.VoiceUDPPortKey: e.value(stack.VoiceUDPPortKey),
		stack.VoiceBundledKey: e.value(stack.VoiceBundledKey),
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
	switch {
	case e.Native && e.Instance != nil:
		e.ps, e.psErr = e.stack().PS(ctx)
	case e.Project == nil:
		e.psErr = errNoProject
	default:
		e.ps, e.psErr = e.Project.PS(ctx)
	}
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
	if ip, ok := settings.DomainIP(d); ok {
		return !publicAddress(ip)
	}
	if d == "" || d == "localhost" || !strings.Contains(d, ".") {
		return true
	}
	return hasDomainSuffix(d, privateSuffixes)
}

var nonPublicRanges = []netip.Prefix{
	netip.MustParsePrefix("100.64.0.0/10"),
	netip.MustParsePrefix("192.0.0.0/24"),
	netip.MustParsePrefix("192.0.2.0/24"),
	netip.MustParsePrefix("198.18.0.0/15"),
	netip.MustParsePrefix("198.51.100.0/24"),
	netip.MustParsePrefix("203.0.113.0/24"),
	netip.MustParsePrefix("240.0.0.0/4"),
	netip.MustParsePrefix("64:ff9b::/96"),
	netip.MustParsePrefix("64:ff9b:1::/48"),
	netip.MustParsePrefix("100::/64"),
	netip.MustParsePrefix("2001:db8::/32"),
	netip.MustParsePrefix("2002::/16"),
	netip.MustParsePrefix("3fff::/20"),
	netip.MustParsePrefix("fec0::/10"),
}

func publicAddress(ip netip.Addr) bool {
	if !ip.IsGlobalUnicast() || ip.IsPrivate() {
		return false
	}
	for _, p := range nonPublicRanges {
		if p.Contains(ip) {
			return false
		}
	}
	return true
}

var (
	privateSuffixes   = []string{".localhost", ".local", ".test", ".internal", ".lan", ".home.arpa", ".example", ".invalid"}
	overlaySuffixes   = []string{".onion", ".i2p", ".alt", ".loki"}
	plainHTTPSuffixes = []string{".onion", ".i2p"}
)

func overlayDomain(domain string) bool {
	return hasDomainSuffix(strings.ToLower(strings.TrimSuffix(domain, ".")), overlaySuffixes)
}

func PlainHTTPDomain(domain string) bool {
	return hasDomainSuffix(strings.ToLower(strings.TrimSuffix(domain, ".")), plainHTTPSuffixes)
}

func hasDomainSuffix(domain string, suffixes []string) bool {
	for _, suffix := range suffixes {
		if strings.HasSuffix(domain, suffix) {
			return true
		}
	}
	return false
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
	if e.Native && e.Instance != nil && e.ApplySettings != nil {
		return e.run(ctx, host.Step{
			Do:  "Writes Fluxer's files from your settings and restarts Fluxer.",
			Run: func(ctx context.Context) error { return e.ApplySettings(ctx, nil) },
		})
	}
	if e.Instance == nil || e.Engine == nil {
		return errNoProject
	}
	if e.loadErr != nil && !e.dryRun {
		return e.loadErr
	}
	return e.run(ctx, host.Step{
		Do: "Writes Fluxer's files from your settings and starts any service that changed or isn't running.",
		Run: func(ctx context.Context) error {
			_, err := deploy.Apply(ctx, e.Instance, e.Engine, deploy.ApplyOptions{})
			return err
		},
	})
}
