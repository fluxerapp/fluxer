// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"maps"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type journal struct{ lines []string }

func (j *journal) add(format string, args ...any) {
	j.lines = append(j.lines, fmt.Sprintf(format, args...))
}

func (j *journal) index(line string) int { return slices.Index(j.lines, line) }

type fakeHost struct {
	name     string
	runtime  Runtime
	j        *journal
	settings map[string]string
	running  map[string]bool
	sick     map[string]bool
	fail     map[string]error
	imp      *fakeImporter
	maxConns int
	missing  map[string]bool
}

func newHost(name string, runtime Runtime, j *journal, imp *fakeHost) *fakeHost {
	h := &fakeHost{name: name, runtime: runtime, j: j, settings: map[string]string{}, running: map[string]bool{}, sick: map[string]bool{}, fail: map[string]error{}, missing: map[string]bool{}}
	if imp != nil {
		h.imp = imp.imp
	} else {
		h.imp = &fakeImporter{j: j, page: "abc"}
	}
	return h
}

func (h *fakeHost) do(op string) error {
	h.j.add("%s %s", h.name, op)
	return h.fail[op]
}

func (h *fakeHost) serviceSet() []string {
	out := []string{"postgres", "nats", "api", "gateway", "media-proxy"}
	if h.runtime == Compose {
		out = append(out, "worker", "edge")
	}
	if !stack.KVPostgres(h.settings) {
		out = append(out, ValkeyService)
	}
	if !stack.FilesystemStorage(h.settings) {
		out = append(out, "seaweedfs")
	}
	if stack.RustSingle(h.settings) {
		out = append(out, stack.RustService)
	} else {
		out = append(out, stack.RustServices...)
	}
	return out
}

func (h *fakeHost) Runtime() Runtime { return h.runtime }
func (h *fakeHost) Dir() string      { return "/srv/" + h.name }
func (h *fakeHost) Backup(context.Context) (string, error) {
	return "/b/" + h.name + ".tar.zst", h.do("backup")
}

func (h *fakeHost) Stop(_ context.Context, s []string) error {
	for _, n := range s {
		delete(h.running, n)
	}
	return h.do("stop " + strings.Join(s, ","))
}

func (h *fakeHost) Halt(context.Context) error {
	clear(h.running)
	return h.do("halt")
}

func (h *fakeHost) Start(context.Context) error {
	for _, n := range h.serviceSet() {
		h.running[n] = true
	}
	return h.do("start")
}

func (h *fakeHost) StartOnly(_ context.Context, s []string) error {
	for _, n := range s {
		h.running[n] = true
	}
	return h.do("start only " + strings.Join(s, ","))
}

func (h *fakeHost) Remove(_ context.Context, s []string) error {
	for _, n := range s {
		delete(h.running, n)
	}
	return h.do("remove " + strings.Join(s, ","))
}

func (h *fakeHost) Check(context.Context) error { return h.do("check") }

func (h *fakeHost) Services(context.Context) ([]Service, error) {
	var out []Service
	for _, n := range slices.Sorted(maps.Keys(h.running)) {
		health := Healthy
		if h.sick[n] {
			health = "unhealthy"
		}
		out = append(out, Service{Name: n, Running: true, Health: health})
	}
	return out, nil
}

func (h *fakeHost) Settings() map[string]string { return maps.Clone(h.settings) }

func (h *fakeHost) Apply(_ context.Context, changes map[string]string) error {
	var parts []string
	for _, k := range slices.Sorted(maps.Keys(changes)) {
		parts = append(parts, k+"="+changes[k])
		if changes[k] == "" {
			delete(h.settings, k)
		} else {
			h.settings[k] = changes[k]
		}
	}
	return h.do("apply " + strings.Join(parts, " "))
}

func (h *fakeHost) Env(changes map[string]string) (map[string]string, error) {
	env := map[string]string{KVURLVar: "redis://valkey:6379/0"}
	maps.Copy(env, h.settings)
	maps.Copy(env, changes)
	if h.maxConns > 0 {
		env[ServerMaxConnsVar] = strconv.Itoa(h.maxConns)
	}
	return env, nil
}

func (h *fakeHost) CheckTarget(_ context.Context, changes map[string]string) error {
	return h.do("check target " + strings.Join(slices.Sorted(maps.Keys(changes)), ","))
}

func (h *fakeHost) FreePorts(context.Context, []int) error         { return h.do("free ports") }
func (h *fakeHost) ObjectsRoot() string                            { return "/objects" }
func (h *fakeHost) CheckObjects(context.Context) error             { return h.do("check objects") }
func (h *fakeHost) PrepareObjects(context.Context) error           { return h.do("prepare objects") }
func (h *fakeHost) ReviveValkey(_ context.Context, d string) error { return h.do("revive " + d) }
func (h *fakeHost) DropData(_ context.Context, w string) error     { return h.do("drop " + w) }
func (h *fakeHost) HoldsData(_ context.Context, w string) (bool, error) {
	return !h.missing[w], nil
}
func (h *fakeHost) AppProxyURL() string { return "http://app-proxy:8080" }
func (h *fakeHost) Importer() Importer  { return h.imp }

func (h *fakeHost) RetireValkey(_ context.Context, suffix string) (string, error) {
	if h.runtime != Native {
		return "", nil
	}
	dir := ValkeyRetiredPrefix + suffix
	return dir, h.do("retire " + dir)
}

type fakeImporter struct {
	j        *journal
	page     string
	copied   bool
	target   int
	mismatch bool
	missing  []string
	failBack bool
}

func (f *fakeImporter) Run(_ context.Context, j Job) error {
	var out any
	switch {
	case j.Script == pageScript:
		out = page{Status: 200, CSP: "default-src 'self'", SHA256: f.page}
	case j.Script == attachmentScript:
		out = attachmentReport{Checked: 2 + len(f.missing), OK: 2, Missing: f.missing}
	case j.Args[0] == ImportKV:
		f.j.add("import %s", strings.Join(append(slices.Clone(j.Args), envArgs(j)...), " "))
		verifyOnly := slices.Contains(j.Args, "--verify-only")
		rep := kvReport{Verified: (!verifyOnly || f.copied) && !(verifyOnly && f.failBack && slices.Contains(j.Args, "--to")), Kinds: []kvDigest{{Kind: "hash", Keys: 4}}}
		if !rep.Verified {
			rep.TargetKinds = []kvDigest{{Kind: "hash", Keys: f.target}}
		}
		if !verifyOnly {
			f.copied = true
		}
		out = rep
	case j.Args[0] == ImportStorage:
		f.j.add("import %s objects=%v", strings.Join(j.Args, " "), j.Objects)
		out = storageReport{Verified: !f.mismatch}
	}
	data, _ := json.Marshal(out)
	if _, err := fmt.Fprintf(j.Stdout, "%s\n", data); err != nil {
		return err
	}
	if f.mismatch && j.Script == "" && j.Args[0] == ImportStorage {
		return errors.New("exit status 3")
	}
	return nil
}

func envArgs(j Job) []string {
	var out []string
	for _, a := range j.EnvArgs {
		out = append(out, a[0], "$"+a[1])
	}
	return out
}

type fakeTarget struct {
	j      *journal
	native *fakeHost
	refuse error
}

func (t *fakeTarget) Dir() string { return "/srv/native" }
func (t *fakeTarget) Preflight(context.Context, map[string]string) error {
	t.j.add("target preflight")
	return t.refuse
}
func (t *fakeTarget) Install(context.Context) error { t.j.add("target install"); return nil }
func (t *fakeTarget) Discard(context.Context) error { t.j.add("target discard"); return nil }
func (t *fakeTarget) Restore(_ context.Context, a string) error {
	t.j.add("target restore %s", a)
	return nil
}

func (t *fakeTarget) Open(_ context.Context, chosen map[string]string) (Host, error) {
	maps.Copy(t.native.settings, chosen)
	t.j.add("target open")
	return t.native, nil
}

func init() { pollEvery = time.Millisecond }

func subsets(opts []Option) [][]Option {
	var out [][]Option
	for mask := range 1 << len(opts) {
		var s []Option
		for i, o := range opts {
			if mask&(1<<i) != 0 {
				s = append(s, o)
			}
		}
		out = append(out, s)
	}
	return out
}

func realSteps(picked []Option, target NativeTarget) []Step {
	var out []Step
	for _, o := range picked {
		switch o {
		case OptDocker:
			out = append(out, NewDockerStep(target, State{}))
		case OptKV:
			out = append(out, NewKVStep())
		case OptRust:
			out = append(out, NewRustStep())
		case OptStorage:
			out = append(out, NewStorageStep())
		}
	}
	slices.Reverse(out)
	return out
}

func TestEveryCombination(t *testing.T) {
	for _, runtime := range []Runtime{Compose, Native} {
		for _, picked := range subsets(All) {
			if runtime == Native && slices.Contains(picked, OptDocker) {
				continue
			}
			name := fmt.Sprintf("%s/%v", runtime, picked)
			t.Run(name, func(t *testing.T) {
				j := &journal{}
				origin := newHost(string(runtime), runtime, j, nil)
				nat := newHost("native", Native, j, origin)
				var phases []string
				r := &Run{Host: origin, OnPhase: func(p Phase, o Option) {
					if p == PhaseMigrate {
						phases = append(phases, string(o))
					}
				}}
				if err := Execute(context.Background(), r, realSteps(picked, &fakeTarget{j: j, native: nat})); err != nil {
					t.Fatalf("%v\n%s", err, strings.Join(j.lines, "\n"))
				}
				var want []string
				for _, o := range Order(picked) {
					want = append(want, string(o))
				}
				if !slices.Equal(phases, want) {
					t.Errorf("migrated in order %v, want %v", phases, want)
				}
				final := r.Host.(*fakeHost)
				if wantNative := runtime == Native || slices.Contains(picked, OptDocker); (final.runtime == Native) != wantNative {
					t.Errorf("ended on %s", final.runtime)
				}
				for o, kv := range map[Option][2]string{
					OptKV:      {settings.KVMode, settings.KVPostgres},
					OptRust:    {settings.RustProcess, settings.RustSingle},
					OptStorage: {settings.StorageBackend, settings.StorageFilesystem},
				} {
					if got := final.settings[kv[0]]; (got == kv[1]) != slices.Contains(picked, o) {
						t.Errorf("%s is %q", kv[0], got)
					}
				}
				if slices.Contains(picked, OptStorage) != (final.settings[StorageReadOnlyVar] == "true") {
					t.Errorf("read-only is %q", final.settings[StorageReadOnlyVar])
				}
				if (r.State.Storage != nil) != slices.Contains(picked, OptStorage) || (r.State.KV != nil) != slices.Contains(picked, OptKV) {
					t.Errorf("state %+v", r.State)
				}
				if slices.Contains(picked, OptDocker) {
					halt, restore := j.index("compose halt"), j.index("target restore /b/compose.tar.zst")
					if halt < 0 || !slices.Contains(j.lines[halt:restore], "compose backup") {
						t.Errorf("the move out of Docker restored a backup made while compose still ran:\n%s", strings.Join(j.lines, "\n"))
					}
				}
			})
		}
	}
}

func TestRefusedTargetLeavesDockerAlone(t *testing.T) {
	noBuild := errors.New("no native build")
	for _, picked := range [][]Option{{OptDocker}, All} {
		t.Run(fmt.Sprint(picked), func(t *testing.T) {
			j := &journal{}
			compose := newHost("compose", Compose, j, nil)
			nat := newHost("native", Native, j, compose)
			r := &Run{Host: compose}
			err := Execute(context.Background(), r, realSteps(picked, &fakeTarget{j: j, native: nat, refuse: noBuild}))
			var fail *Failure
			if !errors.As(err, &fail) || fail.Phase != PhasePreflight || !errors.Is(err, noBuild) {
				t.Fatalf("got %v, want a preflight failure", err)
			}
			touched := slices.ContainsFunc(j.lines, func(line string) bool {
				return strings.HasPrefix(line, "compose backup") || strings.HasPrefix(line, "compose stop") || strings.HasPrefix(line, "compose halt") || strings.HasPrefix(line, "target install")
			})
			if r.Backup != "" || touched || !slices.Contains(j.lines, "target preflight") {
				t.Fatalf("touched the instance before refusing:\n%s", strings.Join(j.lines, "\n"))
			}
		})
	}
}

func TestCombinedFailureReturnsToCompose(t *testing.T) {
	j := &journal{}
	compose := newHost("compose", Compose, j, nil)
	nat := newHost("native", Native, j, compose)
	nat.fail["check"] = errors.New("check broke")
	r := &Run{Host: compose}
	err := Execute(context.Background(), r, realSteps(All, &fakeTarget{j: j, native: nat}))
	var f *Failure
	if !errors.As(err, &f) || f.Phase != PhaseCheck || f.Rollback != nil {
		t.Fatalf("got %v", err)
	}
	if r.Host != compose {
		t.Error("the rollback left the run on the native host")
	}
	order := []string{"native remove rust", "target discard", "compose remove api,worker,media-proxy,objects-init", "compose drop objects-data", "compose apply kv.mode="}
	last := -1
	for _, line := range order {
		i := j.index(line)
		if i <= last {
			t.Fatalf("%q out of order in\n%s", line, strings.Join(j.lines, "\n"))
		}
		last = i
	}
	if j.lines[len(j.lines)-1] != "compose start" {
		t.Errorf("ended with %q", j.lines[len(j.lines)-1])
	}
	for _, k := range []string{settings.KVMode, settings.StorageBackend, StorageReadOnlyVar} {
		if v, ok := compose.settings[k]; ok {
			t.Errorf("compose kept %s=%s", k, v)
		}
	}
}

func TestStorageMismatchDropsTheFiles(t *testing.T) {
	j := &journal{}
	h := newHost("compose", Compose, j, nil)
	h.imp.mismatch = true
	r := &Run{Host: h}
	err := Execute(context.Background(), r, []Step{NewStorageStep()})
	var f *Failure
	if !errors.As(err, &f) || f.Phase != PhaseMigrate {
		t.Fatalf("got %v", err)
	}
	if j.index("compose drop objects-data") < 0 || slices.ContainsFunc(j.lines, func(l string) bool { return strings.HasPrefix(l, "compose apply") }) {
		t.Errorf("got\n%s", strings.Join(j.lines, "\n"))
	}
}

func TestStorageWindow(t *testing.T) {
	j := &journal{}
	h := newHost("native", Native, j, nil)
	r := &Run{Host: h}
	if err := Execute(context.Background(), r, []Step{NewStorageStep()}); err != nil {
		t.Fatal(err)
	}
	if j.index("import storage --root /objects objects=true") < 0 || slices.ContainsFunc(j.lines, func(l string) bool { return strings.Contains(l, "--verify-only") }) {
		t.Errorf("imports: %q", j.lines)
	}
	if r.State.Storage.Runtime != Native {
		t.Errorf("recorded runtime %q", r.State.Storage.Runtime)
	}

	elsewhere := *r.State.Storage
	elsewhere.Runtime = Compose
	if err := UndoStorage(context.Background(), h, &State{Storage: &elsewhere}); !errors.Is(err, ErrUndoElsewhere) {
		t.Errorf("undo of a move made on compose: %v", err)
	}
	h.missing[stack.SeaweedFSVolume] = true
	st := r.State
	if err := UndoStorage(context.Background(), h, &st); !errors.Is(err, ErrNoSeaweedFS) || j.index("native drop objects-data") >= 0 {
		t.Errorf("undo without SeaweedFS's data: %v", err)
	}
	delete(h.missing, stack.SeaweedFSVolume)

	h.fail["check"] = errors.New("check broke")
	if err := OpenWrites(context.Background(), h, &st, time.Unix(100, 0)); err == nil {
		t.Fatal("open writes passed a failing check")
	}
	delete(h.fail, "check")
	if _, ok := h.settings[StorageReadOnlyVar]; ok || !st.Storage.Open() {
		t.Error("writes opened without recording it")
	}
	if err := UndoStorage(context.Background(), h, &st); !errors.Is(err, ErrWritesOpen) {
		t.Errorf("undo after opening writes: %v", err)
	}
	st.Storage.OpenedAt = nil
	if err := UndoStorage(context.Background(), h, &st); !errors.Is(err, ErrWritesOpen) {
		t.Errorf("undo with writes open but not recorded: %v", err)
	}

	h.settings[StorageReadOnlyVar] = "true"
	j.lines = nil
	if err := UndoStorage(context.Background(), h, &st); err != nil {
		t.Fatal(err)
	}
	if st.Storage != nil || h.settings[settings.StorageBackend] != "" || j.index("native drop objects-data") < 0 {
		t.Errorf("undo left %+v %v %q", st, h.settings, j.lines)
	}
}

func TestFailedKVUndoStaysOnPostgres(t *testing.T) {
	j := &journal{}
	h := newHost("native", Native, j, nil)
	r := &Run{Host: h, Now: func() time.Time { return time.Date(2026, 11, 8, 10, 0, 0, 0, time.UTC) }}
	if err := Execute(context.Background(), r, []Step{NewKVStep()}); err != nil {
		t.Fatal(err)
	}
	st := r.State
	h.imp.failBack = true
	undo := &Run{Now: func() time.Time { return time.Date(2026, 11, 8, 11, 0, 0, 0, time.UTC) }}
	if err := UndoKV(context.Background(), h, &st, undo); err == nil {
		t.Fatal("undo passed a copy that doesn't match")
	}
	if h.settings[settings.KVMode] != settings.KVPostgres || st.KV.ValkeyDir != "valkey.migrated-20261108-110000" || j.lines[len(j.lines)-1] != "native start" {
		t.Fatalf("settings %v state %+v\n%s", h.settings, st.KV, strings.Join(j.lines, "\n"))
	}
	h.imp.failBack = false
	if err := UndoKV(context.Background(), h, &st, undo); err != nil || st.KV != nil || j.index("native revive valkey.migrated-20261108-110000") < 0 {
		t.Errorf("retry: %v %+v", err, st.KV)
	}
}

func TestOldDataWaitsForABackup(t *testing.T) {
	at := time.Unix(1000, 0)
	opened := at.Add(time.Hour)
	st := State{KV: &Switched{At: at, ValkeyDir: "valkey.migrated-x"}, Storage: &Switched{At: at}}
	got := OldDataOf(st, at.Add(30*time.Minute))
	if len(got) != 2 || !got[0].Ready || got[0].What != "valkey.migrated-x" || got[1].Ready {
		t.Fatalf("got %+v", got)
	}
	st.Storage.OpenedAt = &opened
	if got := OldDataOf(st, at.Add(30*time.Minute)); got[1].Ready {
		t.Error("SeaweedFS was ready to remove without a backup after opening writes")
	}
	got = OldDataOf(st, opened.Add(time.Minute))
	if !got[1].Ready || got[1].What != stack.SeaweedFSVolume {
		t.Errorf("got %+v", got[1])
	}
	j := &journal{}
	h := newHost("compose", Compose, j, nil)
	if err := CleanUp(context.Background(), h, &st, got[1], opened); err != nil || st.Storage.CleanedUpAt == nil || j.index("compose drop seaweedfs-data") < 0 {
		t.Errorf("got %v %q", err, j.lines)
	}
	if got := OldDataOf(st, opened.Add(time.Hour)); len(got) != 1 {
		t.Errorf("cleaned data offered again: %+v", got)
	}
}
