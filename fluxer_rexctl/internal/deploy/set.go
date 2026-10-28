// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"slices"
	"strconv"
	"strings"
	"unicode"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var ErrDeclined = errors.New("deploy: the change was declined")

var errIPv6NotApplied = errors.New("deploy: the network kept its old IPv6 setting")

var derivedVars = map[string][]string{
	settings.Domain:         {"FLUXER_PUBLIC_ORIGIN", "FLUXER_EDGE_SITE_ADDRESS"},
	settings.EdgeHTTPSPort:  {"FLUXER_PUBLIC_PORT", "FLUXER_PUBLIC_ORIGIN"},
	settings.EdgePublicPort: {"FLUXER_PUBLIC_ORIGIN"},
	stack.EmbeddedWorkerVar: {"FLUXER_WORKER_REPLICAS"},
}

func Promised(set *stack.Set, custom []byte, k settings.Key) []string {
	switch k.Name {
	case settings.EngineHost:
		return nil
	case settings.EdgeMode, settings.IPv6, settings.ImagesRegistry, settings.StorageEndpoint:
		return slices.Clone(stack.Services)
	case settings.MemoryProfile:
		return settings.Services(set, custom, append(stack.ProfileKeys(set), stack.SizingKeys()...))
	}
	return settings.Services(set, custom, append(slices.Clone(k.Vars), derivedVars[k.Name]...))
}

func Lookup(set *stack.Set, custom []byte, name string) (settings.Key, error) {
	vars, err := settings.Vars(set, custom)
	if err != nil {
		return settings.Key{}, ui.Wrap(err, fmt.Sprintf("rexctl can't read %s.", instance.CustomCompose), "Fix the file, then try again.")
	}
	k, err := settings.Lookup(set, vars, name, nil)
	if err != nil {
		return settings.Key{}, keyError(err)
	}
	return k, nil
}

func keyError(err error) error {
	var se *settings.Error
	if errors.As(err, &se) {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: se.Problem, Fix: se.Fix, Err: err}
	}
	return err
}

func Set(ctx context.Context, inst *instance.Instance, e *engine.Engine, changes map[string]*string, o ApplyOptions) (*Change, error) {
	keys := slices.Sorted(maps.Keys(changes))
	if err := refuseEjected(inst); err != nil {
		return nil, err
	}
	if inst.State.Pending != nil {
		return nil, ui.Refused("An earlier settings change didn't finish.", "Run rexctl doctor --fix to put the old values back, then try again.")
	}
	if _, ok := changes[settings.EngineHost]; ok {
		if len(changes) > 1 {
			return nil, ui.Usage("Change engine.host on its own, without other settings.")
		}
		if err := refuseUpgrade(inst); err != nil {
			return nil, err
		}
		return setEngineHost(ctx, inst, e, changes[settings.EngineHost])
	}
	if err := checkDrift(inst, false); err != nil {
		return nil, err
	}
	rs, err := ReleaseSet(inst)
	if err != nil {
		return nil, err
	}
	custom, err := CustomFile(inst)
	if err != nil {
		return nil, err
	}
	st := inst.State
	next := maps.Clone(st.Settings)
	var promised, changed []string
	repairs := true
	for _, name := range keys {
		k, err := Lookup(rs, custom, name)
		if err != nil {
			return nil, err
		}
		if k.Kind == settings.Instance {
			return nil, fmt.Errorf("deploy: %s is an instance setting", name)
		}
		v := changes[name]
		current, had := st.Settings[name]
		repairs = repairs && had && validate(k, current) != nil
		if v == nil {
			delete(next, name)
		} else {
			if err := validate(k, *v); err != nil {
				return nil, err
			}
			next[name] = *v
			if k.Kind == settings.Deployment {
				for _, raw := range k.Vars {
					delete(next, raw)
				}
			}
		}
		if differs := (v == nil && had) || (v != nil && (!had || current != *v)); differs {
			if k.Stateful {
				return nil, ui.Refused(
					fmt.Sprintf("rexctl doesn't change %s once Fluxer has stored data with it.", name),
					"Restore a backup made with the value you want with rexctl restore.")
			}
			changed = append(changed, name)
			promised = append(promised, Promised(rs, custom, k)...)
		}
	}
	if maps.Equal(next, st.Settings) {
		return &Change{Keys: keys, Unchanged: true}, nil
	}
	if next[settings.StorageSecret] != st.Settings[settings.StorageSecret] && next[settings.StorageEndpoint] == "" && next[settings.StorageSecret] != "" {
		return nil, ui.Refused("storage.secret_key only applies to your own object store.", "Set storage.endpoint in the same command, for example rexctl config set storage.endpoint=https://s3.example.com storage.secret_key=-.")
	}
	if st.Upgrade != nil && !repairs {
		if err := refuseUpgrade(inst); err != nil {
			return nil, err
		}
	}
	slices.Sort(promised)
	preview := Preview{Keys: changed, Promised: slices.Compact(promised)}
	newEnv, err := Environment(rs, st, next)
	if err != nil {
		return nil, err
	}
	if st.Upgrade != nil {
		oldEnv, _ := Environment(rs, st, st.Settings)
		if err := checkPorts(ctx, e, inst.Project(), oldEnv, newEnv, changed, next[settings.EdgeMode]); err != nil {
			return nil, err
		}
		if o.Confirm != nil && !o.Confirm(preview) {
			return nil, ErrDeclined
		}
		st.Settings = next
		if err := save(inst); err != nil {
			return nil, err
		}
		return &Change{Keys: changed}, nil
	}
	oldEnv, err := Environment(rs, st, st.Settings)
	if err != nil {
		return nil, err
	}
	staged, err := stageOverlays(inst, rs, next)
	if err != nil {
		return nil, err
	}
	rendered := false
	defer func() {
		if !rendered {
			for _, p := range staged {
				os.Remove(p)
			}
		}
	}()
	newP, err := e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: composeFiles(inst, next), Env: newEnv})
	if err != nil {
		return nil, &ui.ExitError{Code: ui.ExitRefused, Err: err,
			Msg: "Fluxer can't start with that value.",
			Fix: fmt.Sprintf("Run rexctl config get %s to see what it takes, then try again.", keys[0])}
	}
	oldP, err := e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: composeFiles(inst, st.Settings), Env: oldEnv})
	if err != nil {
		return nil, err
	}
	if err := checkPostgres(oldP, newP); err != nil {
		return nil, err
	}
	live, err := running(ctx, inst, e)
	if err != nil {
		return nil, err
	}
	ipv6 := parseBool(st.Settings[settings.IPv6]) != parseBool(next[settings.IPv6])
	if live {
		if err := oldP.Pull(ctx, o.OnEvent); err != nil {
			return nil, err
		}
		plan, err := newP.Plan(ctx)
		if err != nil {
			return nil, err
		}
		preview.Restarts, preview.Removes, preview.Networks = restarted(plan), slices.Clone(plan.Remove), slices.Clone(plan.Networks)
		if ipv6 {
			preview.Restarts = newP.Services()
		}
	}
	if err := checkPorts(ctx, e, inst.Project(), oldEnv, newEnv, changed, next[settings.EdgeMode]); err != nil {
		return nil, err
	}
	if o.Confirm != nil && !o.Confirm(preview) {
		return nil, ErrDeclined
	}

	if !live {
		if ipv6 {
			if err := e.Named(inst.Project(), inst.Dir).Down(context.WithoutCancel(ctx), false); err != nil {
				return nil, err
			}
		}
		st.Settings = next
		if _, err := render(inst, rs, next); err != nil {
			return nil, err
		}
		rendered = true
		if err := save(inst); err != nil {
			return nil, err
		}
		return &Change{Keys: changed}, nil
	}

	if err := newP.Pull(ctx, o.OnEvent); err != nil {
		return nil, err
	}
	st.Pending = &instance.Pending{Settings: next, Keys: changed, StartedAt: now()}
	if err := save(inst); err != nil {
		st.Pending = nil
		return nil, err
	}
	_, err = render(inst, rs, next)
	rendered = err == nil
	if err == nil {
		err = save(inst)
	}
	if err == nil {
		err = bringUp(ctx, oldP, newP, ipv6, o.OnEvent)
	}
	if err != nil {
		if o.Failed != nil {
			o.Failed(newP, err)
		}
		rerr := revert(ctx, inst, rs, oldP, ipv6, o.OnEvent)
		return &Change{Keys: changed, Reverted: rerr == nil}, errors.Join(err, rerr)
	}
	st.Settings = next
	st.Pending = nil
	if err := save(inst); err != nil {
		return nil, err
	}
	return &Change{Keys: changed, Restarted: preview.Restarts, Removed: preview.Removes, Applied: true}, nil
}

func checkPorts(ctx context.Context, e *engine.Engine, project string, oldEnv, newEnv map[string]string, changed []string, mode string) error {
	added, _ := portDiff(oldEnv, newEnv)
	if err := checkPortsFree(ctx, e, project, added); err != nil {
		return err
	}
	if portKeyChanged(changed) {
		return checkPortsDistinct(effectivePorts(newEnv, mode))
	}
	return nil
}

func bringUp(ctx context.Context, oldP, newP *engine.Project, ipv6 bool, onEvent func(engine.Event)) error {
	bg := context.WithoutCancel(ctx)
	if ipv6 {
		if err := oldP.Down(bg, false); err != nil {
			return err
		}
	}
	if err := up(ctx, newP, true, onEvent); err != nil {
		return err
	}
	if !ipv6 {
		return nil
	}
	plan, err := newP.Plan(bg)
	if err != nil {
		return err
	}
	if len(plan.Networks) > 0 {
		return errIPv6NotApplied
	}
	return nil
}

func revert(ctx context.Context, inst *instance.Instance, rs *stack.Set, oldP *engine.Project, ipv6 bool, onEvent func(engine.Event)) error {
	bg := context.WithoutCancel(ctx)
	inst.State.Pending = nil
	if _, err := render(inst, rs, inst.State.Settings); err != nil {
		return err
	}
	if err := save(inst); err != nil {
		return err
	}
	if ipv6 {
		if err := oldP.Down(bg, false); err != nil {
			return err
		}
	}
	return up(bg, oldP, true, onEvent)
}

func stageOverlays(inst *instance.Instance, rs *stack.Set, chosen map[string]string) ([]string, error) {
	var created []string
	for _, o := range stack.OverlaysFor(chosen) {
		name := stack.OverlayFiles[o]
		target := inst.Generated(name)
		if _, err := os.Stat(target); err == nil {
			continue
		}
		data, err := stack.ReadFile(rs, name)
		if err == nil {
			if !inst.OldLayout() {
				data = stack.Managed(name, data)
			}
			err = os.WriteFile(target, data, 0o644)
		}
		if err != nil {
			for _, p := range created {
				os.Remove(p)
			}
			return nil, ui.Wrap(err, "Couldn't write Fluxer's files.", "Check that you can write to "+inst.Dir+", then try again.")
		}
		created = append(created, target)
	}
	return created, nil
}

func checkPostgres(oldP, newP *engine.Project) error {
	const svc = "postgres"
	before, after := oldP.Image(svc), newP.Image(svc)
	if before == "" || after == "" || before == after {
		return nil
	}
	was, err1 := stack.PostgresMajor(before)
	will, err2 := stack.PostgresMajor(after)
	if err1 != nil || err2 != nil || was == will {
		return nil
	}
	return ui.Refused(
		fmt.Sprintf("Your database runs Postgres %d and that image has Postgres %d.", was, will),
		fmt.Sprintf("Use a Postgres %d image. rexctl upgrade moves the database to a new major version when a release needs it.", was))
}

func setEngineHost(ctx context.Context, inst *instance.Instance, e *engine.Engine, v *string) (*Change, error) {
	keys := []string{settings.EngineHost}
	st := inst.State
	if v == nil {
		if st.Engine.Host == "" {
			return &Change{Keys: keys, Unchanged: true}, nil
		}
		st.Engine.Host = ""
		if err := save(inst); err != nil {
			return nil, err
		}
		return &Change{Keys: keys, Applied: true}, nil
	}
	for _, k := range settings.Named() {
		if k.Name == settings.EngineHost {
			if err := validate(k, *v); err != nil {
				return nil, err
			}
		}
	}
	if *v == st.Engine.Host {
		return &Change{Keys: keys, Unchanged: true}, nil
	}
	target, err := engine.Connect(ctx, engine.ConnectOptions{Host: *v, AllowOld: true})
	if err != nil {
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't connect to an engine at %s.", *v), "Check the address and that the engine is running, then try again.")
	}
	defer target.Close()
	if target.ID != e.ID {
		there, err := target.Volumes(ctx, inst.Project())
		if err != nil {
			return nil, err
		}
		here, err := e.Volumes(ctx, inst.Project())
		if err != nil {
			return nil, err
		}
		if len(there) == 0 && len(here) > 0 {
			return nil, ui.Refused("That engine has none of this instance's data.", "Move the instance with rexctl backup, then rexctl restore on the other engine.")
		}
	}
	st.Engine = instance.EngineRef{Kind: string(target.Kind), Host: *v, ID: target.ID}
	if err := save(inst); err != nil {
		return nil, err
	}
	return &Change{Keys: keys, Applied: true}, nil
}

func validate(k settings.Key, v string) error {
	if k.Validate == nil {
		return nil
	}
	err := k.Validate(v)
	switch {
	case err == nil:
		return nil
	case k.Secret:
		return ui.Usage(fmt.Sprintf("That value doesn't work for %s. %s", k.Name, err.Error()))
	}
	shown := strconv.Quote(v)
	if strings.IndexFunc(v, unicode.IsControl) < 0 {
		shown = `"` + v + `"`
	}
	return ui.Usage(fmt.Sprintf("%s can't be %s. %s", k.Name, shown, err.Error()))
}

func (p Preview) Larger() bool {
	return len(p.Networks) > 0 || !subset(p.Restarts, p.Promised) || !subset(p.Removes, p.Promised)
}

func (p Preview) Notes() []string {
	var out []string
	for _, k := range settings.Named() {
		if k.Confirm != "" && slices.Contains(p.Keys, k.Name) && !slices.Contains(out, k.Confirm) {
			out = append(out, k.Confirm)
		}
	}
	return out
}

func (p Preview) NeedsConfirm() bool { return p.Larger() || len(p.Notes()) > 0 }

func subset(items, of []string) bool {
	for _, s := range items {
		if !slices.Contains(of, s) {
			return false
		}
	}
	return true
}

func parseBool(v string) bool {
	b, _ := strconv.ParseBool(v)
	return b
}
