// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const holdPoll = 100 * time.Millisecond

type nativeHost struct {
	app  *App
	inst *instance.Instance
	n    *native.Stack
	set  *stack.Set
}

func (h *nativeHost) Runtime() migrate.Runtime    { return migrate.Native }
func (h *nativeHost) Dir() string                 { return h.inst.Dir }
func (h *nativeHost) Settings() map[string]string { return maps.Clone(h.inst.State.Settings) }
func (h *nativeHost) AppProxyURL() string {
	return "http://" + net.JoinHostPort(h.n.Loopback(), nativeAppProxyPort)
}

func (h *nativeHost) ObjectsRoot() string                  { return filepath.Join(h.n.Root(), "objects") }
func (h *nativeHost) PrepareObjects(context.Context) error { return nil }
func (h *nativeHost) Importer() migrate.Importer {
	return migrate.NativeImporter{Stack: h.n, Set: h.set}
}

func (h *nativeHost) Backup(ctx context.Context) (string, error) {
	return nativeBackup(ctx, h.app, h.inst, h.n, h.set, "Backing up your instance", "migrate")
}

func (h *nativeHost) running(ctx context.Context) bool {
	return h.n.OwnsUnit(ctx) && h.n.ActiveState(ctx) == "active"
}

func (h *nativeHost) live(ctx context.Context) []string {
	if !h.running(ctx) {
		return nil
	}
	entries, err := h.n.Entries(ctx)
	if err != nil {
		return nil
	}
	var out []string
	for _, e := range entries {
		out = append(out, e.Name)
	}
	return out
}

func (h *nativeHost) Stop(ctx context.Context, services []string) error {
	live := h.live(ctx)
	single := h.n.RustSingle()
	var stop []string
	for _, s := range services {
		if name := stack.RunsIn(s, single); slices.Contains(live, name) && !slices.Contains(stop, name) {
			stop = append(stop, name)
		}
	}
	if len(stop) == 0 {
		return nil
	}
	return h.n.Stop(ctx, stop...)
}

func (h *nativeHost) Remove(ctx context.Context, services []string) error {
	return h.Stop(ctx, services)
}

func (h *nativeHost) Halt(ctx context.Context) error { return h.n.Halt(ctx) }

func (h *nativeHost) Start(ctx context.Context) error { return migrateStart(ctx, h.app, h.inst) }

func (h *nativeHost) StartOnly(ctx context.Context, services []string) error {
	single := h.n.RustSingle()
	var want []string
	for _, s := range services {
		if name := stack.RunsIn(s, single); !slices.Contains(want, name) {
			want = append(want, name)
		}
	}
	var held []string
	if h.running(ctx) {
		if entries, err := h.n.Entries(ctx); err == nil {
			for _, e := range entries {
				if e.State == "stopped" && !slices.Contains(want, e.Name) {
					held = append(held, e.Name)
				}
			}
		}
	}
	a, err := rt.ApplyNative(ctx, h.inst, h.n, h.set, h.app.UI.Warn, native.ApplyChanges)
	if err != nil {
		return err
	}
	if !a.Started && !a.Restarted {
		return rt.NativeError(h.n.Start(ctx, want...), "Couldn't start "+ui.JoinAnd(services)+".")
	}
	if err := h.hold(ctx, held); err != nil {
		return err
	}
	return rt.NativeReadyError(h.n, h.n.WaitReady(ctx, engine.DefaultReadyTimeout, nil, held...))
}

func (h *nativeHost) hold(ctx context.Context, held []string) error {
	if len(held) == 0 {
		return nil
	}
	deadline := time.Now().Add(engine.DefaultReadyTimeout)
	for {
		st := h.n.Status(ctx)
		if st.Unit == "active" && st.PSErr == nil {
			return rt.NativeError(h.n.Stop(ctx, held...), "Couldn't keep "+ui.JoinAnd(held)+" stopped.")
		}
		if time.Now().After(deadline) {
			return rt.NativeReadyError(h.n, &engine.NotReadyError{Readiness: st.Readiness, Fatal: st.Fatal})
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(holdPoll):
		}
	}
}

func (h *nativeHost) Check(ctx context.Context) error {
	return h.app.UI.Check(ctx, "Checking Fluxer", func(ctx context.Context, s *ui.Step) error {
		return rt.NativeWait(ctx, h.n, s, "Starting")
	})
}

func (h *nativeHost) Services(ctx context.Context) ([]migrate.Service, error) {
	if !h.running(ctx) {
		return nil, nil
	}
	entries, err := h.n.Entries(ctx)
	if err != nil {
		return nil, err
	}
	var out []migrate.Service
	for _, e := range entries {
		running := e.State == "running" || e.State == "starting"
		out = append(out, migrate.Service{Name: e.Name, Running: running, Health: e.Health})
		for _, w := range e.Watch {
			out = append(out, migrate.Service{Name: e.Name + "/" + w.Name, Running: running, Health: w.Health})
		}
	}
	return out, nil
}

func (h *nativeHost) Apply(_ context.Context, changes map[string]string) error {
	h.inst.State.Settings = merged(h.inst.State.Settings, changes)
	return saveInstance(h.inst)
}

func (h *nativeHost) Env(changes map[string]string) (map[string]string, error) {
	return deploy.Environment(h.set, h.inst.State, merged(h.inst.State.Settings, changes))
}

func (h *nativeHost) CheckTarget(_ context.Context, changes map[string]string) error {
	if changes[settings.RustProcess] == settings.RustSingle {
		bin := filepath.Join(h.n.Current(), "bin", "fluxer-server")
		if _, err := os.Stat(bin); err != nil {
			return &ui.ExitError{Code: ui.ExitRefused, Msg: fmt.Sprintf("This Fluxer release has no %s, which one Rust process needs.", bin), Fix: "Run rexctl upgrade, then try again.", Err: err}
		}
	}
	return nil
}

func (h *nativeHost) FreePorts(_ context.Context, ports []int) error {
	var busy []string
	for _, p := range ports {
		l, err := net.Listen("tcp", net.JoinHostPort(h.n.Loopback(), strconv.Itoa(p)))
		if err != nil {
			busy = append(busy, strconv.Itoa(p))
			continue
		}
		l.Close()
	}
	if len(busy) > 0 {
		return ui.Failed(fmt.Sprintf("Ports %s are still in use after rexctl stopped the Rust services.", strings.Join(busy, ", ")), "Run rexctl status to see what still runs, then try again.")
	}
	return nil
}

func (h *nativeHost) CheckObjects(context.Context) error {
	root := h.ObjectsRoot()
	if err := checkObjectsFS(root); err != nil {
		return err
	}
	if err := migrate.CheckObjectsDir(root); err != nil {
		return err
	}
	dir, _ := h.n.DataDir(stack.SeaweedFSVolume)
	return migrate.CheckSpace(dir, root)
}

func (h *nativeHost) RetireValkey(ctx context.Context, suffix string) (string, error) {
	dir, _ := h.n.DataDir(stack.ValkeyVolume)
	if _, err := os.Stat(dir); errors.Is(err, os.ErrNotExist) {
		return "", nil
	}
	if err := h.Stop(ctx, []string{migrate.ValkeyService}); err != nil {
		return "", err
	}
	name := migrate.ValkeyRetiredPrefix + suffix
	return name, os.Rename(dir, filepath.Join(h.n.Root(), name))
}

func (h *nativeHost) ReviveValkey(_ context.Context, name string) error {
	dir, _ := h.n.DataDir(stack.ValkeyVolume)
	src := filepath.Join(h.n.Root(), filepath.Base(name))
	entries, err := os.ReadDir(dir)
	held := err == nil && len(entries) > 0
	if _, err := os.Stat(src); errors.Is(err, os.ErrNotExist) && held {
		return nil
	}
	if held {
		return ui.Failed(fmt.Sprintf("%s already holds data, so rexctl can't put the old Valkey data back there.", dir), "Run rexctl doctor to see what's wrong.")
	}
	if err := os.Remove(dir); err != nil && !errors.Is(err, os.ErrNotExist) {
		return err
	}
	return os.Rename(src, dir)
}

func (h *nativeHost) HoldsData(_ context.Context, what string) (bool, error) {
	dir, ok := h.n.DataDir(what)
	if !ok {
		return false, ui.Failed(fmt.Sprintf("rexctl doesn't know where %s keeps its data.", what), "")
	}
	entries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, err
	}
	return slices.ContainsFunc(entries, func(e os.DirEntry) bool { return e.Name() != "lost+found" }), nil
}

func (h *nativeHost) DropData(_ context.Context, what string) error {
	dir, ok := h.n.DataDir(what)
	if !ok {
		if !strings.HasPrefix(what, migrate.ValkeyRetiredPrefix) || filepath.Base(what) != what {
			return ui.Refused(fmt.Sprintf("rexctl doesn't delete %s.", what), "")
		}
		return os.RemoveAll(filepath.Join(h.n.Root(), what))
	}
	entries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.Name() == "lost+found" {
			continue
		}
		if err := os.RemoveAll(filepath.Join(dir, e.Name())); err != nil {
			return err
		}
	}
	return nil
}
