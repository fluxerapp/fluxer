// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	goruntime "runtime"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	runtimeSystemd = "systemd"
	runtimeDocker  = "docker"
	runtimePodman  = "podman"
)

const noRuntimeFix = "Install Docker, or run this as root on Debian 12 or later, or Ubuntu 22.04 or later."

type runtimeFacts struct {
	Linux       bool
	Systemd     bool
	NativeOS    bool
	Root        bool
	Interactive bool
	Engine      engine.Kind
	Docker      bool
	Podman      bool
}

func (f runtimeFacts) native() bool { return f.Linux && f.Systemd && f.NativeOS }

func (f runtimeFacts) pick() string {
	switch {
	case f.native() && f.Root:
		return runtimeSystemd
	case f.native() && f.Interactive && f.Engine == "" && (f.Docker || !f.Podman):
		return runtimeSystemd
	case f.Engine != "":
		return string(f.Engine)
	case f.Linux && f.Podman && !f.Docker:
		return runtimePodman
	}
	return runtimeDocker
}

func (f runtimeFacts) choices() []string {
	var out []string
	if f.native() && f.Root {
		out = append(out, runtimeSystemd)
	}
	out = append(out, runtimeDocker)
	if f.Linux && f.Podman {
		out = append(out, runtimePodman)
	}
	return out
}

var nativeReleases = map[string][2]int{"debian": {12, 0}, "ubuntu": {22, 4}}

func nativeOS(h *host.Info) bool {
	if h == nil || h.OS != "linux" {
		return false
	}
	floor, ok := nativeReleases[h.Distro]
	if !ok {
		return false
	}
	major, minor, _ := strings.Cut(h.DistroVersion, ".")
	a, err := strconv.Atoi(major)
	if err != nil {
		return false
	}
	b, _ := strconv.Atoi(minor)
	return a > floor[0] || a == floor[0] && b >= floor[1]
}

func gatherFacts(ctx context.Context, interactive, probe bool) runtimeFacts {
	h, _ := host.Detect(ctx, "")
	f := runtimeFacts{
		Linux:       goruntime.GOOS == "linux",
		Systemd:     host.BootUnitSupported(),
		NativeOS:    nativeOS(h),
		Root:        euid() == 0,
		Interactive: interactive,
		Docker:      dockerInstalled(),
	}
	if _, err := exec.LookPath("podman"); err == nil {
		f.Podman = true
	}
	if !probe || f.native() && f.Root {
		return f
	}
	cctx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	if e, err := engine.Connect(cctx, engine.ConnectOptions{AllowOld: true}); err == nil {
		if !e.TooOld {
			f.Engine = e.Kind
		}
		e.Close()
	}
	return f
}

func runtimeWord(r string) string {
	switch r {
	case runtimeSystemd:
		return "a systemd service"
	case runtimePodman:
		return rt.ContainerLabel(engine.Podman)
	}
	return rt.ContainerLabel(engine.Docker)
}

func (in *installer) runtimeRow() ui.KV {
	key, value := "Runs in", runtimeWord(in.f.Runtime)
	if in.native {
		key = "Runs as"
		value += ", " + native.UnitName(in.f.Project)
	}
	row := ui.KV{Key: key, Value: value}
	if in.resuming == nil && len(in.facts.choices()) > 1 {
		row.Edit = 'r'
	}
	return row
}

func (in *installer) askRuntime(ctx context.Context, c *ui.Checklist) error {
	choices := in.facts.choices()
	if in.machineRuns == 0 || !in.app.UI.Interactive() || len(choices) < 2 {
		return nil
	}
	opts := []ui.Option{}
	for _, r := range choices {
		label := runtimeWord(r)
		if r == runtimeSystemd {
			label = "A systemd service on this machine"
		}
		o := ui.Option{Label: label, Value: r}
		if r == in.f.Runtime {
			opts = append([]ui.Option{o}, opts...)
			continue
		}
		opts = append(opts, o)
	}
	choice, err := c.Recover("Where should Fluxer run?", opts)
	if err != nil || choice == in.f.Runtime {
		return err
	}
	return in.setRuntime(ctx, choice)
}

func (in *installer) setRuntime(ctx context.Context, r string) error {
	for _, e := range []*engine.Engine{in.engine, in.found} {
		if e != nil {
			e.Close()
		}
	}
	in.engine, in.found, in.findErr, in.rel = nil, nil, nil, nil
	in.ran = map[string]bool{}
	in.f.Runtime = r
	in.native = r == runtimeSystemd
	in.container = !in.native
	in.f.Engine = ""
	if in.container {
		in.f.Engine = r
		return nil
	}
	if err := rt.NativeAlone(ctx); err != nil {
		return err
	}
	return in.nativeTarget()
}

func (in *installer) defaultRuntime(ctx context.Context) {
	in.facts = gatherFacts(ctx, in.app.UI.Interactive(), true)
	in.pickRuntime()
}

func (in *installer) pickRuntime() {
	f := in.f
	containerOnly := len(changedFlags(in.c, "setup-podman", "install-docker")) > 0
	pick := in.facts.pick()
	if containerOnly && pick == runtimeSystemd {
		pick = runtimeDocker
		if in.facts.Engine != "" {
			pick = string(in.facts.Engine)
		}
	}
	f.Runtime = pick
	in.defaulted = true
	if pick != runtimeSystemd {
		f.Engine = pick
	}
}

var movingFrom string

func others(dir string) []*instance.Instance {
	out := instance.Others(dir)
	if movingFrom == "" {
		return out
	}
	return slices.DeleteFunc(out, func(o *instance.Instance) bool { return instance.RealPath(o.Dir) == movingFrom })
}

func sameDomain(dir, domain string) *instance.Instance {
	for _, o := range others(dir) {
		if domain != "" && o.Domain() == domain {
			return o
		}
	}
	return nil
}

func domainTaken(dir, domain string) error {
	if o := sameDomain(dir, normaliseDomain(domain)); o != nil {
		return ui.Refused(fmt.Sprintf("The Fluxer in %s already serves %s.", o.Dir, o.Domain()), "Run rexctl status --dir "+shellWord(o.Dir)+" to check it, or pick another address for a second Fluxer.")
	}
	return nil
}

func nameTaken(name, dir string, nativeRuntime bool) bool {
	for _, o := range others(dir) {
		if o.Project() == name {
			return true
		}
	}
	return nativeRuntime && nativeNameTaken(name)
}

func nativeNameTaken(name string) bool {
	if movingFrom != "" {
		return native.SettingsTaken(name)
	}
	return native.NameTaken(name)
}

func nameRefusal(name, dir string, nativeRuntime bool) error {
	switch {
	case native.ReservedName(name):
		return ui.Usage(fmt.Sprintf("rexctl can't use %s as a name, because Fluxer keeps its own files under it.", name))
	case nameTaken(name, dir, nativeRuntime):
		return ui.Refused(fmt.Sprintf("Another Fluxer on this machine is called %s.", name), "Pass --project with another name, such as "+name+"2.")
	}
	return nil
}

func freeName(dir string, explicitDir, nativeRuntime bool) string {
	var candidates []string
	candidates = append(candidates, defaultProject)
	if explicitDir {
		candidates = append(candidates, filepath.Base(dir))
	}
	for n := 2; n < 100; n++ {
		candidates = append(candidates, defaultProject+strconv.Itoa(n))
	}
	for _, c := range candidates {
		if projectPattern.MatchString(c) && !native.ReservedName(c) && !nameTaken(c, dir, nativeRuntime) {
			return c
		}
	}
	return ""
}

func (in *installer) secondDir(defaultDir string) (string, error) {
	parent := filepath.Dir(defaultDir)
	if in.c.Flags().Changed("project") {
		if err := nameRefusal(in.f.Project, defaultDir, in.native); err != nil {
			return "", err
		}
		dir := filepath.Join(parent, in.f.Project)
		if _, err := os.Stat(dir); err == nil {
			return "", ui.Refused(fmt.Sprintf("%s is already there.", dir), "Pass --dir with an empty folder for a second Fluxer.")
		}
		return dir, nil
	}
	for n := 2; n < 100; n++ {
		name := defaultProject + strconv.Itoa(n)
		dir := filepath.Join(parent, name)
		if _, err := os.Stat(dir); err == nil || nameTaken(name, dir, in.native) {
			continue
		}
		in.f.Project = name
		return dir, nil
	}
	return "", ui.Refused(fmt.Sprintf("Fluxer is already installed in %s.", defaultDir), "Pass --dir with an empty folder and --project with a new name for a second Fluxer.")
}

func (in *installer) pickName(explicitDir bool) error {
	if in.c.Flags().Changed("project") {
		return nameRefusal(in.f.Project, in.dir, in.native)
	}
	if name := freeName(in.dir, explicitDir, in.native); name != "" {
		in.f.Project = name
		return nil
	}
	return ui.Refused("Every name rexctl tries is taken on this machine.", "Pass --project with a new name.")
}

func otherNativeLoopbacks(dir string) ([]string, bool) {
	var taken []string
	found := false
	for _, o := range others(dir) {
		if !o.State.IsNative() {
			continue
		}
		found = true
		taken = append(taken, native.New(o).Loopback())
	}
	if !found && nativeNameTaken(native.FirstName) {
		found = true
	}
	if host.StandaloneNativeInUse(context.Background()) {
		found = true
	}
	return taken, found
}

func pickLoopback(dir string) (string, error) {
	taken, others := otherNativeLoopbacks(dir)
	if !others {
		return "", nil
	}
	if addr := native.FreeLoopback(append(taken, "127.0.0.1")); addr != "" {
		return addr, nil
	}
	return "", fmt.Errorf("no free loopback address from 127.0.0.2 to 127.0.0.254")
}

func aliasLine(app *App, old, now string) {
	app.UI.Info("%s is now %s.", old, now)
}

func changedFlags(c *cobra.Command, names ...string) []string {
	var out []string
	for _, name := range names {
		if c.Flags().Changed(name) {
			out = append(out, name)
		}
	}
	return out
}

func keptData(dir string) error {
	if instance.Recorded(dir) {
		return nil
	}
	return ui.Refused(fmt.Sprintf("%s still holds a Fluxer that rexctl removed and kept the data of.", dir), "Run rexctl start --dir "+shellWord(dir)+" to run it again. For a second Fluxer next to it, pass --dir with an empty folder.")
}
