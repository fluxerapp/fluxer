// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"bytes"
	"errors"
	"fmt"
	"io/fs"
	"maps"
	"os"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const EnvBeforeRexctl = ".env.before-rexctl"

func HandRunEnvChanges(inst *Instance) (map[string]string, error) {
	h, err := inst.handRunFolder()
	if err != nil {
		return nil, err
	}
	set, err := inst.Set()
	if err != nil {
		return nil, err
	}
	custom, err := inst.Custom()
	if err != nil {
		return nil, err
	}
	read, err := settings.Vars(set, custom)
	if err != nil {
		return nil, err
	}
	own, err := inst.Environment()
	if err != nil {
		return nil, err
	}
	defaults := composeDefaults(setFiles(set, custom))
	out := map[string]string{}
	for _, v := range read {
		have, ok := h.Env[v]
		want, wok := own[v]
		switch {
		case wok && ok && want == have, wok && !ok && want == "":
		case wok && !ok && equalsDefault(defaults[v], want, own):
		case !wok && (!ok || have == ""):
		default:
			out[v] = want
		}
	}
	var names []string
	for _, f := range inst.ComposeFiles() {
		names = append(names, relPath(inst.Dir, stackRootName(inst, f)))
	}
	if !slices.Equal(names, h.Files) {
		sep := h.Env["COMPOSE_PATH_SEPARATOR"]
		if sep == "" {
			sep = string(os.PathListSeparator)
		}
		out["COMPOSE_FILE"] = strings.Join(names, sep)
	}
	return out, nil
}

func stackRootName(inst *Instance, path string) string {
	if rest, ok := strings.CutPrefix(path, inst.Generated("")); ok {
		return inst.Path(strings.TrimPrefix(rest, string(os.PathSeparator)))
	}
	return path
}

func SyncHandRunEnv(inst *Instance, changes map[string]string, now time.Time) ([]string, error) {
	if len(changes) == 0 {
		return nil, nil
	}
	path := inst.Path(EnvFileName)
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	before := inst.Path(EnvBeforeRexctl)
	switch old, err := os.ReadFile(before); {
	case err == nil && !bytes.Equal(old, data):
		return nil, ui.Refused(fmt.Sprintf("%s is already there and differs from .env.", before), "Move it out of the folder, then run this again.")
	case errors.Is(err, fs.ErrNotExist):
		if err := envfile.WriteFileAtomic(before, data, true); err != nil {
			return nil, err
		}
	case err != nil:
		return nil, err
	}
	f, err := envfile.Parse(path, data)
	if err != nil && !errors.Is(err, envfile.ErrDuplicateKey) {
		return nil, err
	}
	var added []string
	for _, k := range slices.Sorted(maps.Keys(changes)) {
		if !plainEnvValue(changes[k]) {
			return nil, ui.Refused(fmt.Sprintf("rexctl can't write the value of %s into .env.", k), "Run rexctl migrate --undo kv or rexctl migrate --undo storage to go back, then run this again.")
		}
		if _, ok := f.Get(k); ok {
			f.Set(k, changes[k])
		} else {
			added = append(added, k)
		}
	}
	if len(added) > 0 {
		f.Comment("Set by rexctl migrate on " + now.Format("2 January 2006") + " to match the running containers")
		for _, k := range added {
			f.Set(k, changes[k])
		}
	}
	var placed []string
	undo := func(cause error) error {
		for _, p := range placed {
			os.Remove(p)
		}
		return errors.Join(cause, envfile.WriteFileAtomic(path, data, false), envfile.WriteFileAtomic(inst.Path(StateDirName, HandRunDir, EnvFileName), data, true))
	}
	for _, from := range inst.ComposeFiles() {
		to := stackRootName(inst, from)
		if to == from || isFile(to) {
			continue
		}
		stock, err := os.ReadFile(from)
		if err == nil {
			err = envfile.WriteFileAtomic(to, stock, false)
		}
		if err != nil {
			return nil, undo(err)
		}
		placed = append(placed, to)
	}
	if err := envfile.WriteFileAtomic(path, f.Bytes(), false); err != nil {
		return nil, undo(err)
	}
	h, err := inst.handRunFolder()
	if err != nil {
		return nil, undo(err)
	}
	hr, err := h.hashes(inst.State.HandRun.At)
	if err != nil {
		return nil, undo(err)
	}
	for _, name := range append(h.recorded(), EnvFileName) {
		if err := copyHandRunFile(inst.Path(name), inst.Path(StateDirName, HandRunDir, name)); err != nil {
			return nil, undo(err)
		}
	}
	old := inst.State.HandRun
	inst.State.HandRun = hr
	if err := inst.Save(); err != nil {
		inst.State.HandRun = old
		return nil, undo(err)
	}
	names := make([]string, len(placed))
	for i, p := range placed {
		names[i] = relPath(inst.Dir, p)
	}
	return names, nil
}

func plainEnvValue(v string) bool {
	return !strings.ContainsAny(v, "'\"\\\n\r$`#") && strings.TrimSpace(v) == v
}
