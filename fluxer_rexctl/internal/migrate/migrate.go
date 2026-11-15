// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"errors"
	"fmt"
	"io"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

type Option string

const (
	OptDocker  Option = "docker"
	OptKV      Option = "kv"
	OptRust    Option = "rust"
	OptStorage Option = "storage"
)

var order = []Option{OptKV, OptStorage, OptDocker, OptRust}

var All = []Option{OptDocker, OptKV, OptRust, OptStorage}

func (o Option) Title() string {
	switch o {
	case OptDocker:
		return "Out of Docker"
	case OptKV:
		return "Valkey to Postgres"
	case OptRust:
		return "One Rust process"
	case OptStorage:
		return "SeaweedFS to plain files"
	}
	return string(o)
}

func (o Option) Summary() string {
	switch o {
	case OptDocker:
		return "Fluxer runs as one systemd service with no containers, and Docker or Podman is no longer needed for it."
	case OptKV:
		return "Cache, rate limits and queues move into the Postgres database, and the Valkey service goes away."
	case OptRust:
		return "Eight Rust services run in one process. A failing service restarts on its own inside it."
	case OptStorage:
		return "Uploaded files become plain files on disk, and the SeaweedFS services go away. Once you confirm the moved files and new uploads arrive, going back to SeaweedFS needs a new migration."
	}
	return ""
}

type Runtime string

const (
	Compose Runtime = instance.RuntimeCompose
	Native  Runtime = instance.RuntimeNative
)

var ErrUnknownOption = errors.New("migrate: unknown option")

func Parse(list string) ([]Option, error) {
	var out []Option
	for part := range strings.SplitSeq(list, ",") {
		o := Option(strings.TrimSpace(part))
		if o == "" {
			continue
		}
		if !slices.Contains(All, o) {
			return nil, fmt.Errorf("%w %q, use %s", ErrUnknownOption, o, joinOptions(All))
		}
		if !slices.Contains(out, o) {
			out = append(out, o)
		}
	}
	return Order(out), nil
}

func joinOptions(opts []Option) string {
	s := make([]string, len(opts))
	for i, o := range opts {
		s[i] = string(o)
	}
	return strings.Join(s, ", ")
}

func Order(opts []Option) []Option {
	out := make([]Option, 0, len(opts))
	for _, o := range order {
		if slices.Contains(opts, o) {
			out = append(out, o)
		}
	}
	return out
}

type Mode struct {
	Option Option
	Key    string
	Env    string
	Value  string
}

var Modes = []Mode{
	{OptKV, settings.KVMode, "FLUXER_KV_MODE", settings.KVPostgres},
	{OptRust, settings.RustProcess, "FLUXER_RUST_PROCESS", settings.RustSingle},
	{OptStorage, settings.StorageBackend, "FLUXER_STORAGE_BACKEND", settings.StorageFilesystem},
}

func Done(runtime Runtime, values map[string]string) []Option {
	var out []Option
	if runtime == Native {
		out = append(out, OptDocker)
	}
	for _, d := range Modes {
		v := strings.TrimSpace(values[d.Key])
		if v == "" {
			v = strings.TrimSpace(values[d.Env])
		}
		if v == d.Value {
			out = append(out, d.Option)
		}
	}
	return Order(out)
}

type Phase string

const (
	PhasePreflight Phase = "preflight"
	PhaseBackup    Phase = "backup"
	PhaseStop      Phase = "stop writers"
	PhaseMigrate   Phase = "migrate"
	PhaseVerify    Phase = "verify"
	PhaseSwitch    Phase = "switch"
	PhaseStart     Phase = "start"
	PhaseCheck     Phase = "check"
	PhaseRollback  Phase = "rollback"
)

type Step interface {
	Option() Option
	Writers() []string
	Preflight(ctx context.Context, r *Run) error
	Migrate(ctx context.Context, r *Run) error
	Verify(ctx context.Context, r *Run) error
	Switch(ctx context.Context, r *Run) error
	Check(ctx context.Context, r *Run) error
	Rollback(ctx context.Context, r *Run) error
}

type Service struct {
	Name    string
	Running bool
	Health  string
}

const Healthy = "healthy"

type Host interface {
	Runtime() Runtime
	Dir() string
	Backup(ctx context.Context) (string, error)
	Stop(ctx context.Context, services []string) error
	Halt(ctx context.Context) error
	Start(ctx context.Context) error
	StartOnly(ctx context.Context, services []string) error
	Remove(ctx context.Context, services []string) error
	Check(ctx context.Context) error
	Services(ctx context.Context) ([]Service, error)
	Settings() map[string]string
	Apply(ctx context.Context, changes map[string]string) error
	Env(changes map[string]string) (map[string]string, error)
	CheckTarget(ctx context.Context, changes map[string]string) error
	FreePorts(ctx context.Context, ports []int) error
	ObjectsRoot() string
	CheckObjects(ctx context.Context) error
	PrepareObjects(ctx context.Context) error
	RetireValkey(ctx context.Context, suffix string) (string, error)
	ReviveValkey(ctx context.Context, dir string) error
	HoldsData(ctx context.Context, what string) (bool, error)
	DropData(ctx context.Context, what string) error
	AppProxyURL() string
	Importer() Importer
}

type Run struct {
	Host    Host
	Backup  string
	Done    []Option
	Picked  []Option
	State   State
	Notes   []string
	Now     func() time.Time
	Log     io.Writer
	OnPhase func(Phase, Option)
}

func (r *Run) note(format string, args ...any) {
	r.Notes = append(r.Notes, fmt.Sprintf(format, args...))
}

func (r *Run) now() time.Time {
	if r.Now != nil {
		return r.Now().UTC()
	}
	return time.Now().UTC()
}

func (r *Run) phase(p Phase, o Option) {
	if r.OnPhase != nil {
		r.OnPhase(p, o)
	}
}

type Failure struct {
	Phase    Phase
	Option   Option
	Err      error
	Rollback error
}

func (f *Failure) Error() string {
	where := string(f.Phase)
	if f.Option != "" {
		where = string(f.Option) + " " + where
	}
	msg := fmt.Sprintf("migrate: %s failed: %v", where, f.Err)
	if f.Rollback != nil {
		msg += fmt.Sprintf(", and the rollback failed too: %v", f.Rollback)
	}
	return msg
}

func (f *Failure) Unwrap() error { return f.Err }

func Execute(ctx context.Context, r *Run, steps []Step) error {
	steps = sortSteps(steps)
	r.Picked = nil
	for _, s := range steps {
		r.Picked = append(r.Picked, s.Option())
	}
	for _, s := range steps {
		r.phase(PhasePreflight, s.Option())
		if err := s.Preflight(ctx, r); err != nil {
			return &Failure{Phase: PhasePreflight, Option: s.Option(), Err: err}
		}
	}
	r.phase(PhaseBackup, "")
	path, err := r.Host.Backup(ctx)
	if err != nil {
		return &Failure{Phase: PhaseBackup, Err: err}
	}
	r.Backup = path

	var writers []string
	for _, s := range steps {
		for _, w := range s.Writers() {
			if !slices.Contains(writers, w) {
				writers = append(writers, w)
			}
		}
	}
	origin := r.Host
	r.phase(PhaseStop, "")
	if err := origin.Stop(ctx, writers); err != nil {
		return r.fail(ctx, &Failure{Phase: PhaseStop, Err: err}, nil, origin)
	}

	var touched []Step
	for _, s := range steps {
		touched = append(touched, s)
		for _, ph := range []struct {
			phase Phase
			fn    func(context.Context, *Run) error
		}{{PhaseMigrate, s.Migrate}, {PhaseVerify, s.Verify}, {PhaseSwitch, s.Switch}} {
			r.phase(ph.phase, s.Option())
			if err := ph.fn(ctx, r); err != nil {
				return r.fail(ctx, &Failure{Phase: ph.phase, Option: s.Option(), Err: err}, touched, origin)
			}
		}
		r.Done = append(r.Done, s.Option())
	}

	r.phase(PhaseStart, "")
	if err := r.Host.Start(ctx); err != nil {
		return r.fail(ctx, &Failure{Phase: PhaseStart, Err: err}, touched, origin)
	}
	r.phase(PhaseCheck, "")
	if err := r.Host.Check(ctx); err != nil {
		return r.fail(ctx, &Failure{Phase: PhaseCheck, Err: err}, touched, origin)
	}
	for _, s := range steps {
		r.phase(PhaseCheck, s.Option())
		if err := s.Check(ctx, r); err != nil {
			return r.fail(ctx, &Failure{Phase: PhaseCheck, Option: s.Option(), Err: err}, touched, origin)
		}
	}
	return nil
}

func (r *Run) fail(ctx context.Context, f *Failure, touched []Step, origin Host) error {
	ctx = context.WithoutCancel(ctx)
	var errs []error
	for _, s := range slices.Backward(touched) {
		r.phase(PhaseRollback, s.Option())
		if err := s.Rollback(ctx, r); err != nil {
			errs = append(errs, fmt.Errorf("%s: %w", s.Option(), err))
		}
	}
	r.Done = nil
	r.State = State{}
	r.Host = origin
	r.phase(PhaseStart, "")
	if err := origin.Start(ctx); err != nil {
		errs = append(errs, fmt.Errorf("starting again: %w", err))
	}
	f.Rollback = errors.Join(errs...)
	return f
}

func sortSteps(steps []Step) []Step {
	out := slices.Clone(steps)
	slices.SortStableFunc(out, func(a, b Step) int {
		return slices.Index(order, a.Option()) - slices.Index(order, b.Option())
	})
	return out
}
