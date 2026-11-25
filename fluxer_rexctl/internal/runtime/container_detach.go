// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	ComposeFileName = "compose.yaml"
	EnvFileName     = "fluxer.env"
	composeService  = "fluxer"
)

var detachDropped = []string{"FLUXER_SIZING_SOURCE", "FLUXER_BACKUP_ON_UPGRADE", "FLUXER_INSTALL_METHOD", "FLUXER_UPGRADE_COMMAND"}

func ComposeYAML(spec engine.SingleSpec, project string, podman bool) []byte {
	tool := "docker compose"
	if podman {
		tool = "podman compose"
	}
	var b strings.Builder
	b.WriteString("# Fluxer in one container. The guide is at https://docs.fluxer.com/operator/docker/\n")
	b.WriteString("# Start     " + tool + " up -d\n")
	b.WriteString("# Admin     " + tool + " exec fluxer fluxerd setup-token\n")
	b.WriteString("# Upgrade   " + tool + " exec fluxer fluxerd version prints the command\n")
	if project != "" {
		b.WriteString("name: " + project + "\n")
	}
	b.WriteString("services:\n")
	b.WriteString("  " + composeService + ":\n")
	b.WriteString("    image: " + spec.Image + "\n")
	b.WriteString("    restart: " + spec.Restart + "\n")
	b.WriteString("    stop_grace_period: " + strconv.Itoa(spec.StopTimeout) + "s\n")
	b.WriteString("    env_file: " + EnvFileName + "\n")
	b.WriteString("    environment:\n")
	b.WriteString("      FLUXER_INSTALL_METHOD: compose\n")
	if podman {
		b.WriteString("    sysctls:\n")
		for _, k := range slices.Sorted(maps.Keys(spec.Sysctls)) {
			b.WriteString("      " + k + ": " + spec.Sysctls[k] + "\n")
		}
	}
	if len(spec.ExtraHosts) > 0 {
		b.WriteString("    extra_hosts:\n")
		for _, h := range spec.ExtraHosts {
			fmt.Fprintf(&b, "      - %q\n", h)
		}
	}
	b.WriteString("    ports:\n")
	for _, p := range spec.Ports {
		fmt.Fprintf(&b, "      - %q\n", p.String())
	}
	b.WriteString("    volumes:\n")
	b.WriteString("      - " + spec.Volume + ":" + spec.VolumeTarget + "\n")
	b.WriteString("\nvolumes:\n")
	b.WriteString("  " + spec.Volume + ":\n")
	b.WriteString("    name: " + spec.Volume + "\n")
	return []byte(b.String())
}

var (
	ErrMultiline  = errors.New("runtime: a setting holds more than one line")
	ErrUnquotable = errors.New("runtime: a setting ends in a backslash before a quote")
)

const (
	backupScheduleVar = "FLUXER_BACKUP_SCHEDULE"
	backupKeepVar     = "FLUXER_BACKUP_KEEP"
)

func envQuote(v string) (string, error) {
	if strings.HasSuffix(v, `\`) || strings.Contains(v, `\'`) {
		return "", ErrUnquotable
	}
	return "'" + strings.ReplaceAll(v, "'", `\'`) + "'", nil
}

func detachedEnv(env map[string]string, set *stack.Set) ([]byte, error) {
	var b strings.Builder
	b.WriteString("# Fluxer settings, written by rexctl detach. Every setting is at https://docs.fluxer.com/operator/settings/\n")
	for _, k := range slices.Sorted(maps.Keys(env)) {
		v := env[k]
		if slices.Contains(detachDropped, k) {
			continue
		}
		if s, ok := set.Setting(k); ok && s.Default == v {
			continue
		}
		if strings.ContainsAny(v, "\r\n") {
			return nil, fmt.Errorf("%w: %s", ErrMultiline, k)
		}
		q, err := envQuote(v)
		if err != nil {
			return nil, fmt.Errorf("%w: %s", err, k)
		}
		b.WriteString(k + "=" + q + "\n")
	}
	return []byte(b.String()), nil
}

func PodmanUpgradeCommand() string {
	image := "ghcr.io/fluxerapp/fluxer:{version}"
	return `sed -i.bak "s#ghcr.io/fluxerapp/fluxer:[^[:space:]]*#` + image + `#" compose.yaml && grep -q '` + image + `' compose.yaml && podman compose pull && podman compose up -d`
}

var (
	ErrDetachStartedAgain = errors.New("runtime: rexctl started its own container again")
	ErrDetachLeftStopped  = errors.New("runtime: rexctl couldn't start its own container again")
)

func removeIfThere(path string) error {
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return nil
}

type Detached struct {
	ComposeFile string
	EnvFile     string
}

type DetachOptions struct {
	Rexctl   string
	Schedule string
	Keep     int
}

func Detach(ctx context.Context, r Runtime, o DetachOptions) (Detached, error) {
	c, ok := r.(*containerRT)
	if !ok {
		return Detached{}, ErrUnsupported
	}
	return c.detach(ctx, o)
}

func (r *containerRT) detach(ctx context.Context, o DetachOptions) (Detached, error) {
	out := Detached{ComposeFile: r.inst.Path(ComposeFileName), EnvFile: r.inst.Path(EnvFileName)}
	e, err := r.engine(ctx)
	if err != nil {
		return out, err
	}
	eng, ok := e.(*engine.Engine)
	if !ok {
		return out, ErrUnsupported
	}
	set, err := deploy.ReleaseSet(r.inst)
	if err != nil {
		return out, err
	}
	env, err := r.environment(set)
	if err != nil {
		return out, err
	}
	if o.Schedule == "daily" || o.Schedule == "weekly" {
		env[backupScheduleVar] = o.Schedule
		if o.Keep > 0 {
			env[backupKeepVar] = strconv.Itoa(o.Keep)
		}
	}
	envData, err := detachedEnv(env, set)
	if err != nil {
		return out, err
	}
	if r.kind == engine.Podman {
		q, err := envQuote(PodmanUpgradeCommand())
		if err != nil {
			return out, err
		}
		envData = append(envData, "FLUXER_UPGRADE_COMMAND="+q+"\n"...)
	}
	spec := ContainerSpec(r.inst, r.inst.State.Release, env)
	compose := ComposeYAML(spec, r.inst.Project(), r.kind == engine.Podman)
	if _, err := r.push(ctx, e, set); err != nil {
		return out, err
	}
	written := func(err error) (Detached, error) {
		return out, errors.Join(err, removeIfThere(out.ComposeFile), removeIfThere(out.EnvFile))
	}
	if err := envfile.WriteFileAtomic(out.EnvFile, envData, true); err != nil {
		return written(err)
	}
	if err := envfile.WriteFileAtomic(out.ComposeFile, compose, false); err != nil {
		return written(err)
	}
	p, err := eng.LoadPublished(ctx, engine.Spec{Name: r.inst.Project(), Dir: r.inst.Dir, Files: []string{out.ComposeFile}})
	if err != nil {
		return written(err)
	}
	startAgain := func(err error, recreate bool) (Detached, error) {
		bg := context.WithoutCancel(ctx)
		_, ferr := written(nil)
		var serr error
		if recreate {
			serr = r.create(bg, e, ContainerSpec(r.inst, r.inst.State.Release, env), set)
		}
		if serr == nil {
			serr = e.StartSingle(bg, r.name())
		}
		if serr != nil {
			return out, errors.Join(ErrDetachLeftStopped, err, serr, ferr)
		}
		return out, errors.Join(ErrDetachStartedAgain, err, ferr)
	}
	for i, name := range []string{r.name(), PreviousName(r.inst)} {
		if err := e.StopSingle(ctx, name, ContainerStop); err != nil {
			return startAgain(err, i > 0)
		}
		if err := e.RemoveSingle(ctx, name); err != nil {
			return startAgain(err, i > 0)
		}
	}
	if err := p.Up(ctx, engine.UpOptions{Recreate: engine.RecreateChanged}); err != nil {
		_ = p.Down(context.WithoutCancel(ctx), false)
		return startAgain(err, true)
	}
	r.inst.State.Ejected = &instance.Ejected{At: time.Now().UTC(), Rexctl: o.Rexctl}
	return out, saveState(r.inst)
}
