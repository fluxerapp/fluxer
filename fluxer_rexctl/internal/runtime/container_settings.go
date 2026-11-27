// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bytes"
	"context"
	"errors"
	"io"
	"maps"
	"path/filepath"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type containerSettings struct{ r *containerRT }

const (
	checkDir     = "/tmp"
	checkFile    = ".rexctl-check.env"
	checkCurrent = ".rexctl-current.env"
)

func (containerSettings) Supports(key string) (bool, string) {
	if settings.DockerOnly(key) {
		return false, key + " only changes the older Docker Compose layout, and this instance runs in one container."
	}
	if why, ok := native.Unsupported(key); ok {
		return false, key + " has no effect on this instance. " + why
	}
	return true, ""
}

func (c containerSettings) Validate(ctx context.Context, p *deploy.Prepared) error {
	if p == nil || p.Unchanged {
		return nil
	}
	var names, reasons []string
	for _, k := range p.Keys {
		if ok, why := c.Supports(k); !ok {
			names, reasons = append(names, k), append(reasons, why)
		}
	}
	switch len(names) {
	case 0:
	case 1:
		return unsupported(reasons[0], "Leave it out.")
	default:
		return unsupported(ui.JoinAnd(names)+" have no effect on this instance, because it runs in one container.", "Leave them out.")
	}
	if err := bindProblem(p.Settings); err != nil {
		return err
	}
	if err := c.bundledProblem(p); err != nil {
		return err
	}
	if err := c.hostPath(p); err != nil {
		return err
	}
	if err := deploy.CheckHostPorts(ctx, c.r.inst, p); err != nil {
		return err
	}
	out, err := c.runCheck(ctx, p, false)
	var ee *engine.ExitError
	switch {
	case err == nil, errors.Is(err, errNoCheck):
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

func (c containerSettings) bundledProblem(p *deploy.Prepared) error {
	env, _, err := c.candidate(p)
	if err != nil {
		return ContainerError(err, "rexctl can't work out Fluxer's settings with that change.")
	}
	get := func(k string) string { return strings.TrimSpace(env[k]) }
	if get(stack.KVModeVar) == settings.KVStandalone && get("FLUXER_KV_URL") == "" {
		return ui.Refused("Fluxer's container has no Valkey, so kv.mode standalone needs your own Valkey server in FLUXER_KV_URL.",
			"Set kv.mode to postgres, or set FLUXER_KV_URL to your Valkey server.")
	}
	if get("FLUXER_STORAGE_BACKEND") == settings.StorageS3 && get("FLUXER_S3_ENDPOINT") == "" {
		return ui.Refused("Fluxer's container has no SeaweedFS, so storage.backend s3 needs your own object store in storage.endpoint.",
			"Set storage.backend to filesystem, or set storage.endpoint to your object store.")
	}
	return nil
}

func (c containerSettings) hostPath(p *deploy.Prepared) error {
	for _, k := range p.Keys {
		if !native.PathKey(k) {
			continue
		}
		for _, w := range strings.Fields(p.Settings[k]) {
			if strings.HasPrefix(w, "/") && !native.MappedPath(w) && k == settings.GeoIPVar {
				return ui.Refused(k+" points at "+w+" on this machine, and Fluxer's container can't see files outside its own folders.",
					"Run rexctl config set "+settings.GeoIPDatabase+"="+w+" instead, and rexctl copies the file into the instance.")
			}
			if strings.HasPrefix(w, "/") && !native.MappedPath(w) {
				return ui.Refused(k+" points at "+w+" on this machine, and Fluxer's container can't see files outside its own folders.",
					"Copy the file into "+c.r.inst.Path(instance.CertsDir)+", then set "+k+" to /certs/"+filepath.Base(w)+".")
			}
		}
	}
	return nil
}

func (c containerSettings) candidate(p *deploy.Prepared) (env, secrets map[string]string, err error) {
	st := c.r.inst.State
	oldSettings, oldSecrets := st.Settings, st.Secrets
	st.Settings, st.Secrets = p.Settings, p.Secrets
	defer func() { st.Settings, st.Secrets = oldSettings, oldSecrets }()
	return c.r.n.Environment(p.Set)
}

func checkEnvData(env, secrets map[string]string) ([]byte, error) {
	all := maps.Clone(env)
	maps.Copy(all, secrets)
	return envfile.MarshalRaw(all)
}

func (c containerSettings) runCheck(ctx context.Context, p *deploy.Prepared, against bool) (string, error) {
	r := c.r
	e, err := r.engine(ctx)
	if err != nil {
		return "", errNoCheck
	}
	cur, ok, err := e.Inspect(ctx, r.name())
	if err != nil || !ok || !cur.Running() {
		return "", errNoCheck
	}
	env, secrets, err := c.candidate(p)
	if err != nil {
		return "", ContainerError(err, "rexctl can't work out Fluxer's settings with that change.")
	}
	data, err := checkEnvData(env, secrets)
	if err != nil {
		return "", err
	}
	files := []engine.File{{Name: checkFile, Data: data, Mode: 0o600}}
	args := []string{"fluxerd", "check-config", "--config", checkDir + "/" + checkFile}
	if against {
		env, secrets, err := r.n.Environment(p.Set)
		if err != nil {
			return "", ContainerError(err, "rexctl can't work out Fluxer's settings.")
		}
		data, err := checkEnvData(env, secrets)
		if err != nil {
			return "", err
		}
		files = append(files, engine.File{Name: checkCurrent, Data: data, Mode: 0o600})
		args = append(args, "--against", checkDir+"/"+checkCurrent)
	}
	if err := e.WriteSingleFiles(ctx, r.name(), checkDir, ContainerUID, ContainerUID, files...); err != nil {
		return "", errNoCheck
	}
	defer func() {
		_ = e.ExecSingle(context.WithoutCancel(ctx), r.name(), engine.ExecOptions{}, "rm", "-f", checkDir+"/"+checkFile, checkDir+"/"+checkCurrent)
	}()
	var out bytes.Buffer
	cctx, cancel := context.WithTimeout(ctx, checkConfigTimeout)
	defer cancel()
	err = e.ExecSingle(cctx, r.name(), engine.ExecOptions{Stdout: &out, Stderr: &out}, args...)
	if ctx.Err() != nil {
		return "", ctx.Err()
	}
	return out.String(), err
}

func (c containerSettings) Preview(ctx context.Context, p *deploy.Prepared) (Preview, error) {
	if p == nil || p.Unchanged {
		return Preview{}, nil
	}
	pv := Preview{Notes: p.Preview.Notes()}
	env, secrets, err := c.r.n.Environment(p.Set)
	if err != nil {
		return pv, ContainerError(err, "rexctl can't work out Fluxer's settings.")
	}
	nextEnv, nextSecrets, err := c.candidate(p)
	if err != nil {
		return pv, ContainerError(err, "rexctl can't work out Fluxer's settings with that change.")
	}
	if maps.Equal(env, nextEnv) && maps.Equal(secrets, nextSecrets) {
		return pv, nil
	}
	pv.Restarts = []string{"all"}
	if !slices.Equal(ContainerPorts(c.r.inst, env), portsWith(c.r, p, nextEnv)) {
		return pv, nil
	}
	if out, err := c.runCheck(ctx, p, true); err == nil {
		if parts, ok := reloadParts(out); ok {
			pv.Restarts = parts
		}
	}
	return pv, nil
}

func portsWith(r *containerRT, p *deploy.Prepared, env map[string]string) []engine.PortMap {
	st := r.inst.State
	old := st.Settings
	st.Settings = p.Settings
	defer func() { st.Settings = old }()
	return ContainerPorts(r.inst, env)
}

func (c containerSettings) Apply(ctx context.Context, s *ui.Step, p *deploy.Prepared) (*deploy.Change, error) {
	r := c.r
	st := r.inst.State
	if p.Unchanged {
		return &deploy.Change{Keys: p.Keys, Unchanged: true}, nil
	}
	if st.Upgrade != nil || st.Stopped {
		st.Settings, st.Secrets = p.Settings, p.Secrets
		return &deploy.Change{Keys: p.Preview.Keys}, saveState(r.inst)
	}
	apply := func(ctx context.Context, title string) ([]string, error) {
		return r.applySettings(ctx, s, title)
	}
	logs := func(ctx context.Context, w io.Writer) error {
		return r.Logs(ctx, w, LogOptions{Tail: startLogLines})
	}
	return applyPending(ctx, r.inst, p, apply, logs)
}

func (r *containerRT) applySettings(ctx context.Context, s *ui.Step, title string) ([]string, error) {
	a, err := r.apply(ctx, applyChanges)
	if err != nil {
		return nil, ContainerError(err, "Couldn't apply the settings to Fluxer's container.")
	}
	if a.Created || a.Started || a.Restarted {
		return []string{"all"}, r.wait(ctx, s, title)
	}
	restarted := partsOf(a.Reloaded)
	if len(restarted) == 0 || slices.Equal(restarted, []string{"edge"}) {
		s = nil
	}
	return restarted, r.wait(ctx, s, title)
}

func (c containerSettings) Render(ctx context.Context, w io.Writer) error {
	set, err := deploy.ReleaseSet(c.r.inst)
	if err != nil {
		return err
	}
	env, secrets, err := c.r.n.Environment(set)
	if err != nil {
		return ContainerError(err, "rexctl can't work out Fluxer's settings.")
	}
	var b bytes.Buffer
	for _, k := range slices.Sorted(maps.Keys(env)) {
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

func resolveContainerPending(ctx context.Context, r *containerRT, s *ui.Step) error {
	if r.inst.State.Pending == nil {
		return nil
	}
	return resolvePending(ctx, r.inst, func(ctx context.Context, title string) ([]string, error) {
		return r.applySettings(ctx, s, title)
	})
}
