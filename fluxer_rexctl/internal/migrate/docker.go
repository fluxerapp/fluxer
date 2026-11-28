// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var (
	errAlreadyNative   = errors.New("migrate: the instance already runs in the new layout")
	errStorageReadOnly = errors.New("migrate: uploads are still read-only after the move off SeaweedFS, open them with rexctl migrate --open-writes or go back with rexctl migrate --undo storage first")
	errNoConverter     = errors.New("migrate: this instance can't move Valkey or SeaweedFS data during the move")
)

var ErrNoNativeEquivalent = errors.New("migrate: a setting has no equivalent without Docker")

var ErrOneWay = errors.New("migrate: the new release already started on your own PostgreSQL database, and the old containers can't run on it")

type MoveTarget interface {
	Dir() string
	Runtime() string
	Preflight(ctx context.Context, chosen map[string]string) error
	Prepare(ctx context.Context) error
	Install(ctx context.Context) error
	Restore(ctx context.Context, archive string) error
	Units() []SavedUnit
	Mounts() []DataMount
	Open(ctx context.Context, chosen map[string]string) (Host, error)
	Discard(ctx context.Context) error
	OneWay() bool
}

var composeDefaults = map[string]string{
	settings.StorageBackend: settings.StorageS3,
	settings.KVMode:         settings.KVStandalone,
	settings.LiveKitEnabled: "true",
	StorageReadOnlyVar:      "",
}

type Converter interface {
	ConvertedBackup(ctx context.Context, opts []Option) (string, error)
}

func WithModes(current map[string]string, opts []Option) map[string]string {
	out := maps.Clone(current)
	if out == nil {
		out = map[string]string{}
	}
	for _, m := range Modes {
		if slices.Contains(opts, m.Option) {
			out[m.Key] = m.Value
		}
	}
	return out
}

type NativeSettings struct {
	Settings map[string]string
	Left     []string
}

func ForNative(compose map[string]string) (NativeSettings, error) {
	kept, left, refused := forNative(compose)
	out := NativeSettings{Settings: kept, Left: left}
	if len(refused) > 0 {
		return out, fmt.Errorf("%w: Fluxer in the new layout doesn't use %s. Remove them with rexctl config unset %s first", ErrNoNativeEquivalent, strings.Join(refused, ", "), strings.Join(refused, " "))
	}
	return out, nil
}

func Unmovable(compose map[string]string) []string {
	_, _, refused := forNative(compose)
	return refused
}

func forNative(compose map[string]string) (map[string]string, []string, []string) {
	rest := maps.Clone(compose)
	maps.DeleteFunc(rest, func(k, v string) bool {
		return strings.TrimSpace(v) == "" && strings.ToUpper(k) == k && !strings.ContainsAny(k, ".- ")
	})
	for k := range composeDefaults {
		delete(rest, k)
	}
	delete(rest, settings.RustProcess)
	kept, left, refused := settings.ForNative(settings.NativeSet(stack.EmbeddedRelease()), rest)
	kept[settings.RustProcess] = settings.RustSingle
	for k, def := range composeDefaults {
		v := strings.TrimSpace(compose[k])
		if v == "" {
			v = def
		}
		kept[k] = v
	}
	return kept, left, refused
}

type DockerStep struct {
	target    MoveTarget
	recorded  State
	convert   []Option
	installed bool
	converted bool
}

func NewDockerStep(target MoveTarget, recorded State, convert []Option) *DockerStep {
	return &DockerStep{target: target, recorded: recorded, convert: convert}
}

func (s *DockerStep) Option() Option    { return OptDocker }
func (s *DockerStep) Writers() []string { return nil }

func (s *DockerStep) Preflight(ctx context.Context, r *Run) error {
	if r.Host.Runtime() != Compose {
		return errAlreadyNative
	}
	if st := s.recorded.Storage; st != nil && !st.Open() {
		return errStorageReadOnly
	}
	c, err := ForNative(WithModes(r.Host.Settings(), s.convert))
	if err != nil {
		return err
	}
	if len(c.Left) > 0 {
		r.note("These settings only change the old containers and stay with them: %s.", strings.Join(c.Left, ", "))
	}
	if err := s.convertPreflight(ctx, r); err != nil {
		return err
	}
	return s.target.Preflight(ctx, c.Settings)
}

func (s *DockerStep) convertPreflight(ctx context.Context, r *Run) error {
	if len(s.convert) == 0 {
		return nil
	}
	if _, ok := r.Host.(Converter); !ok {
		return errNoConverter
	}
	if slices.Contains(s.convert, OptKV) {
		if err := kvSource(r.Host); err != nil {
			return err
		}
		if err := verifyKV(ctx, r); err != nil {
			return err
		}
	}
	if slices.Contains(s.convert, OptStorage) {
		return r.Host.CheckObjects(ctx)
	}
	return nil
}

func (s *DockerStep) backup(ctx context.Context, r *Run) (string, error) {
	if len(s.convert) == 0 {
		return r.Host.Backup(ctx)
	}
	c, ok := r.Host.(Converter)
	if !ok {
		return "", errNoConverter
	}
	s.converted = true
	if slices.Contains(s.convert, OptKV) {
		if err := r.Host.StartOnly(ctx, []string{postgresService, ValkeyService}); err != nil {
			return "", err
		}
		kv := &KVStep{}
		if err := kv.Migrate(ctx, r); err != nil {
			return "", err
		}
		if err := kv.Verify(ctx, r); err != nil {
			return "", err
		}
	}
	if slices.Contains(s.convert, OptStorage) {
		if err := r.Host.StartOnly(ctx, []string{seaweedFSService}); err != nil {
			return "", err
		}
		if err := (&StorageStep{}).Migrate(ctx, r); err != nil {
			return "", err
		}
	}
	if err := r.Host.Halt(ctx); err != nil {
		return "", err
	}
	return c.ConvertedBackup(ctx, s.convert)
}

func (s *DockerStep) Migrate(ctx context.Context, r *Run) error {
	if err := r.Host.Halt(ctx); err != nil {
		return err
	}
	archive, err := s.backup(ctx, r)
	if err != nil {
		return err
	}
	s.installed = true
	r.State.Docker = &Moving{At: r.now(), Dir: s.target.Dir(), Runtime: s.target.Runtime(), Mounts: s.target.Mounts()}
	if err := r.record(); err != nil {
		return err
	}
	err = s.target.Install(ctx)
	r.State.Docker.Units = s.target.Units()
	if rerr := r.record(); err == nil {
		err = rerr
	}
	if err != nil {
		return err
	}
	return s.target.Restore(ctx, archive)
}

func (s *DockerStep) Verify(context.Context, *Run) error { return nil }

func (s *DockerStep) Switch(ctx context.Context, r *Run) error {
	c, err := ForNative(WithModes(r.Host.Settings(), s.convert))
	if err != nil {
		return err
	}
	next, err := s.target.Open(ctx, c.Settings)
	if err != nil {
		return err
	}
	if err := r.Host.Halt(ctx); err != nil {
		return err
	}
	if r.State.Docker != nil {
		at := r.now()
		r.State.Docker.SwitchedAt = &at
		if err := r.record(); err != nil {
			return err
		}
	}
	r.Host = next
	return nil
}

func (s *DockerStep) Check(context.Context, *Run) error { return nil }

func (s *DockerStep) Rollback(ctx context.Context, r *Run) error {
	if !s.installed && s.converted && slices.Contains(s.convert, OptStorage) {
		return r.Host.DropData(ctx, stack.ObjectsVolume)
	}
	if !s.installed {
		return nil
	}
	if r.Started && s.target.OneWay() {
		return errors.Join(ErrOneWay, r.Host.Halt(ctx))
	}
	return s.target.Discard(ctx)
}
