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
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var errIPv6NotApplied = errors.New("deploy: the network kept its old IPv6 setting")

var derivedVars = map[string][]string{
	settings.Domain:         {"FLUXER_PUBLIC_ORIGIN", "FLUXER_EDGE_SITE_ADDRESS"},
	settings.EdgeHTTPPort:   {"FLUXER_PUBLIC_PORT", "FLUXER_PUBLIC_ORIGIN"},
	settings.EdgeHTTPSPort:  {"FLUXER_PUBLIC_PORT", "FLUXER_PUBLIC_ORIGIN"},
	"FLUXER_PUBLIC_SCHEME":  {"FLUXER_PUBLIC_PORT", "FLUXER_PUBLIC_ORIGIN", "FLUXER_EDGE_SITE_ADDRESS"},
	settings.EdgePublicPort: {"FLUXER_PUBLIC_ORIGIN"},
	stack.EmbeddedWorkerVar: {"FLUXER_WORKER_REPLICAS"},
}

func Promised(set *stack.Set, custom []byte, k settings.Key) []string {
	switch k.Name {
	case settings.EngineHost:
		return nil
	case settings.EdgeMode, settings.IPv6, settings.ImagesRegistry, settings.StorageEndpoint, settings.VoiceBundled:
		return slices.Clone(stack.Services)
	case settings.MemoryProfile:
		return settings.Services(set, custom, append(stack.ProfileKeys(set), stack.SizingKeys()...))
	case settings.DiskProfile:
		return settings.Services(set, custom, stack.DiskSizingKeys())
	case settings.SignedMedia:
		return settings.Services(set, custom, []string{settings.SignatureModeVar, settings.SignatureSecretsVar})
	}
	return settings.Services(set, custom, append(slices.Clone(k.Vars), derivedVars[k.Name]...))
}

func lookup(set *stack.Set, custom []byte, held map[string]string, name string) (settings.Key, error) {
	vars, err := settings.Vars(set, custom)
	if err != nil {
		return settings.Key{}, ui.Wrap(err, fmt.Sprintf("rexctl can't read %s.", instance.CustomCompose), "Fix the file, then try again.")
	}
	k, err := settings.Lookup(set, settings.WithHeld(vars, held), name, nil)
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

type Prepared struct {
	Set        *stack.Set
	Keys       []string
	Settings   map[string]string
	Secrets    map[string]string
	Preview    Preview
	Unchanged  bool
	Engine     bool
	EngineHost string
	planned    *Preview
}

func Changeable(inst *instance.Instance) error {
	if err := refuseEjected(inst); err != nil {
		return err
	}
	if inst.State.Pending != nil {
		return ui.Refused("An earlier settings change didn't finish.", "Run rexctl doctor --fix to put the old values back, then try again.")
	}
	return nil
}

func Prepare(inst *instance.Instance, changes map[string]*string) (*Prepared, error) {
	keys := slices.Sorted(maps.Keys(changes))
	rs, err := ReleaseSet(inst)
	if err != nil {
		return nil, err
	}
	if v, ok := changes[settings.EngineHost]; ok {
		return prepareEngineHost(inst, rs, keys, v)
	}
	custom, err := CustomFile(inst)
	if err != nil {
		return nil, err
	}
	st := inst.State
	next := maps.Clone(st.Settings)
	var changed []string
	repairs := true
	for _, name := range keys {
		k, err := lookup(rs, custom, st.Settings, name)
		if err != nil {
			return nil, err
		}
		if k.Kind == settings.Instance {
			return nil, fmt.Errorf("deploy: %s is an instance setting", name)
		}
		v := changes[name]
		if v != nil && name == settings.EdgeBind {
			if norm, err := settings.NormaliseBind(*v); err == nil {
				v = &norm
			}
		}
		current, had := st.Settings[name]
		broken := brokenVAPID(st, name)
		repairs = repairs && (broken || had && validate(k, current) != nil)
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
			if k.Stateful && !broken {
				return nil, ui.Refused(
					fmt.Sprintf("rexctl doesn't change %s once Fluxer has stored data with it.", name),
					"Restore a backup made with the value you want with rexctl restore.")
			}
			changed = append(changed, name)
		}
	}
	if maps.Equal(next, st.Settings) {
		return &Prepared{Set: rs, Keys: keys, Unchanged: true}, nil
	}
	if next[settings.StorageSecret] != st.Settings[settings.StorageSecret] && next[settings.StorageEndpoint] == "" && next[settings.StorageSecret] != "" {
		return nil, ui.Refused("storage.secret_key only applies to your own object store.", "Set storage.endpoint in the same command, for example rexctl config set storage.endpoint=https://s3.example.com storage.secret_key=-.")
	}
	if st.Upgrade != nil && !repairs {
		if err := refuseUpgrade(inst); err != nil {
			return nil, err
		}
	}
	nextSecrets, err := signingSecrets(rs, st, next)
	if err != nil {
		return nil, err
	}
	withSecrets := *st
	withSecrets.Secrets = nextSecrets
	if _, err := Environment(rs, &withSecrets, next); err != nil {
		return nil, err
	}
	return &Prepared{Set: rs, Keys: keys, Settings: next, Secrets: nextSecrets, Preview: Preview{Keys: changed}}, nil
}

func brokenVAPID(st *instance.State, name string) bool {
	if name != secrets.VAPIDPublicKey && name != secrets.VAPIDPrivateKey {
		return false
	}
	current := func(key string) string {
		if v, ok := st.Settings[key]; ok {
			return v
		}
		return st.Secrets[key]
	}
	return secrets.ValidateVAPID(current(secrets.VAPIDPublicKey), current(secrets.VAPIDPrivateKey)) != nil
}

func Set(ctx context.Context, inst *instance.Instance, e *engine.Engine, changes map[string]*string, o ApplyOptions) (*Change, error) {
	if err := Changeable(inst); err != nil {
		return nil, err
	}
	pr, err := Prepare(inst, changes)
	if err != nil {
		return nil, err
	}
	return SetPrepared(ctx, inst, e, pr, o)
}

func prepareEngineHost(inst *instance.Instance, rs *stack.Set, keys []string, v *string) (*Prepared, error) {
	if len(keys) > 1 {
		return nil, ui.Usage("Change engine.host on its own, without other settings.")
	}
	host := ""
	if v != nil {
		k, _ := namedKey(settings.EngineHost)
		if err := validate(k, *v); err != nil {
			return nil, err
		}
		host = *v
	}
	p := &Prepared{Set: rs, Keys: keys, Engine: true, EngineHost: host, Unchanged: host == inst.State.Engine.Host}
	if !p.Unchanged {
		p.Preview = Preview{Keys: keys}
	}
	return p, nil
}

func namedKey(name string) (settings.Key, bool) {
	for _, k := range settings.Named() {
		if k.Name == name {
			return k, true
		}
	}
	return settings.Key{}, false
}

func SetPrepared(ctx context.Context, inst *instance.Instance, e *engine.Engine, pr *Prepared, o ApplyOptions) (*Change, error) {
	if pr.Engine {
		if err := refuseUpgrade(inst); err != nil {
			return nil, err
		}
		return setEngineHost(ctx, inst, e, pr.EngineHost)
	}
	if err := checkDrift(inst, false); err != nil {
		return nil, err
	}
	if pr.Unchanged {
		return &Change{Keys: pr.Keys, Unchanged: true}, nil
	}
	st := inst.State
	rs, next, nextSecrets, preview := pr.Set, pr.Settings, pr.Secrets, pr.Preview
	changed := preview.Keys
	if st.Upgrade != nil {
		withSecrets := *st
		withSecrets.Secrets = nextSecrets
		newEnv, err := Environment(rs, &withSecrets, next)
		if err != nil {
			return nil, err
		}
		oldEnv, _ := Environment(rs, st, st.Settings)
		if err := checkPorts(ctx, e, inst.Project(), oldEnv, newEnv, changed, next[settings.EdgeMode]); err != nil {
			return nil, err
		}
		st.Settings, st.Secrets = next, nextSecrets
		if err := save(inst); err != nil {
			return nil, err
		}
		return &Change{Keys: changed}, nil
	}
	pl, err := preflight(ctx, inst, e, pr, true, o.OnEvent)
	if err != nil {
		return nil, err
	}
	rendered := false
	defer func() {
		if !rendered {
			pl.discard()
		}
	}()
	oldP, newP, live, ipv6 := pl.oldP, pl.newP, pl.live, pl.ipv6
	preview = pl.preview
	if pr.planned != nil && grew(*pr.planned, preview) {
		return nil, ui.Refused("The newest Fluxer images restart more of Fluxer than rexctl showed you, so nothing changed.",
			"Run the same command again to see what it restarts now.")
	}

	if !live {
		var stopped []string
		if ipv6 {
			detached, err := detachVoice(ctx, e, inst)
			if err != nil {
				return nil, err
			}
			for _, v := range detached {
				stopped = append(stopped, v.Server.ID)
			}
			if err := downInstance(ctx, e, inst); err != nil {
				return nil, err
			}
		}
		st.Settings, st.Secrets = next, nextSecrets
		if _, err := render(inst, rs, next); err != nil {
			return nil, err
		}
		rendered = true
		if err := save(inst); err != nil {
			return nil, err
		}
		return &Change{Keys: changed, VoiceDown: stopped}, nil
	}

	if err := newP.Pull(ctx, o.OnEvent); err != nil {
		return nil, err
	}
	if slices.Contains(preview.Restarts, edgeService) {
		if err := CheckEdge(ctx, inst, newP, ""); err != nil {
			return nil, err
		}
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
	var detached []AttachedVoice
	if err == nil && ipv6 {
		detached, err = detachVoice(ctx, e, inst)
	}
	if err == nil {
		err = bringUp(ctx, oldP, newP, ipv6, o.OnEvent)
	}
	if err != nil {
		if o.Failed != nil {
			o.Failed(newP, err)
		}
		rerr := revert(ctx, inst, rs, oldP, ipv6, o.OnEvent)
		return &Change{Keys: changed, Reverted: rerr == nil, VoiceDown: attachVoice(ctx, e, detached)}, errors.Join(err, rerr)
	}
	st.Settings, st.Secrets = next, nextSecrets
	st.Pending = nil
	if err := save(inst); err != nil {
		return nil, err
	}
	return &Change{Keys: changed, Restarted: preview.Restarts, Removed: preview.Removes, Applied: true, VoiceDown: attachVoice(ctx, e, detached)}, nil
}

func Plan(ctx context.Context, inst *instance.Instance, e *engine.Engine, pr *Prepared) (Preview, error) {
	if pr.Unchanged || pr.Engine || inst.State.Upgrade != nil {
		return pr.Preview, nil
	}
	pl, err := preflight(ctx, inst, e, pr, false, nil)
	if err != nil {
		return pr.Preview, err
	}
	pl.discard()
	planned := pl.preview
	pr.planned = &planned
	return pl.preview, nil
}

func grew(before, after Preview) bool {
	outside := func(next, prev []string) bool {
		return slices.ContainsFunc(next, func(s string) bool { return !slices.Contains(prev, s) })
	}
	return outside(after.Restarts, before.Restarts) || outside(after.Removes, before.Removes) || outside(after.Networks, before.Networks)
}

type planned struct {
	oldP, newP *engine.Project
	live, ipv6 bool
	preview    Preview
	staged     []string
}

func (pl *planned) discard() {
	for _, p := range pl.staged {
		os.Remove(p)
	}
}

func preflight(ctx context.Context, inst *instance.Instance, e *engine.Engine, pr *Prepared, pull bool, onEvent func(engine.Event)) (pl *planned, err error) {
	st := inst.State
	pl = &planned{preview: pr.Preview}
	withSecrets := *st
	withSecrets.Secrets = pr.Secrets
	newEnv, err := Environment(pr.Set, &withSecrets, pr.Settings)
	if err != nil {
		return nil, err
	}
	oldEnv, err := Environment(pr.Set, st, st.Settings)
	if err != nil {
		return nil, err
	}
	if pl.staged, err = stageOverlays(inst, pr.Set, pr.Settings); err != nil {
		return nil, err
	}
	defer func() {
		if err != nil {
			pl.discard()
			pl = nil
		}
	}()
	pl.newP, err = e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: composeFiles(inst, pr.Settings), Env: newEnv})
	if err != nil {
		return pl, &ui.ExitError{Code: ui.ExitRefused, Err: err,
			Msg: "Fluxer can't start with that value.",
			Fix: fmt.Sprintf("Run rexctl config get %s to see what it takes, then try again.", pr.Keys[0])}
	}
	if pl.oldP, err = e.Load(ctx, engine.Spec{Name: inst.Project(), Dir: inst.Dir, Files: composeFiles(inst, st.Settings), Env: oldEnv}); err != nil {
		return pl, err
	}
	if err = checkPostgres(pl.oldP, pl.newP); err != nil {
		return pl, err
	}
	if pl.live, err = running(ctx, inst, e); err != nil {
		return pl, err
	}
	pl.ipv6 = parseBool(st.Settings[settings.IPv6]) != parseBool(pr.Settings[settings.IPv6])
	if pl.live {
		if pull {
			if err = pl.oldP.Pull(ctx, onEvent); err != nil {
				return pl, err
			}
		}
		var plan *engine.Plan
		if plan, err = pl.newP.Plan(ctx); err != nil {
			return pl, err
		}
		pl.preview.Restarts, pl.preview.Removes, pl.preview.Networks = restarted(plan), slices.Clone(plan.Remove), slices.Clone(plan.Networks)
		if pl.ipv6 {
			pl.preview.Restarts = pl.newP.Services()
		}
	}
	err = checkPorts(ctx, e, inst.Project(), oldEnv, newEnv, pr.Preview.Keys, pr.Settings[settings.EdgeMode])
	return pl, err
}

func signingSecrets(rs *stack.Set, st *instance.State, next map[string]string) (map[string]string, error) {
	env, err := Environment(rs, st, next)
	if err != nil {
		return nil, err
	}
	if !settings.SignsAttachments(env[settings.SignatureModeVar]) || strings.TrimSpace(env[settings.SignatureSecretsVar]) != "" {
		return st.Secrets, nil
	}
	if _, chosen := next[settings.SignatureSecretsVar]; chosen {
		return nil, ui.Refused(
			fmt.Sprintf("%s needs a signing key, and %s is empty.", settings.SignedMedia, settings.SignatureSecretsVar),
			fmt.Sprintf("Run rexctl config unset %s and rexctl makes a key.", settings.SignatureSecretsVar))
	}
	key, err := secrets.Base64(32)
	if err != nil {
		return nil, err
	}
	out := maps.Clone(st.Secrets)
	out[settings.SignatureSecretsVar] = key
	return out, nil
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
	var rendered map[string][]byte
	for _, o := range stack.OverlaysFor(chosen) {
		name := stack.OverlayFiles[o]
		target := inst.Generated(name)
		if _, err := os.Stat(target); err == nil {
			continue
		}
		var err error
		if rendered == nil {
			rendered, err = stack.Files(rs, stack.OverlaysFor(chosen), inst.State.Images.Refs, inst.Project())
		}
		if err == nil {
			data := rendered[name]
			if inst.OldLayout() {
				data = stack.Plain(name, data)
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

func setEngineHost(ctx context.Context, inst *instance.Instance, e *engine.Engine, v string) (*Change, error) {
	keys := []string{settings.EngineHost}
	st := inst.State
	if v == "" {
		if st.Engine.Host == "" {
			return &Change{Keys: keys, Unchanged: true}, nil
		}
		st.Engine.Host = ""
		if err := save(inst); err != nil {
			return nil, err
		}
		return &Change{Keys: keys, Applied: true}, nil
	}
	if v == st.Engine.Host {
		return &Change{Keys: keys, Unchanged: true}, nil
	}
	target, err := engine.Connect(ctx, engine.ConnectOptions{Host: v, AllowOld: true})
	if err != nil {
		return nil, ui.Wrap(err, fmt.Sprintf("rexctl can't connect to an engine at %s.", v), "Check the address and that the engine is running, then try again.")
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
	st.Engine = instance.EngineRef{Kind: string(target.Kind), Host: v, ID: target.ID}
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

func (p Preview) Notes() []string {
	var out []string
	for _, k := range settings.Named() {
		if k.Confirm != "" && slices.Contains(p.Keys, k.Name) && !slices.Contains(out, k.Confirm) {
			out = append(out, k.Confirm)
		}
	}
	return out
}

func parseBool(v string) bool {
	b, _ := strconv.ParseBool(v)
	return b
}
