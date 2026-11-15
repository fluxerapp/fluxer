// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var (
	ErrAlreadyNative   = errors.New("migrate: the instance already runs without Docker")
	ErrStorageReadOnly = errors.New("migrate: uploads are still read-only after the move off SeaweedFS, open them with rexctl migrate --experimental-native --open-writes or go back with rexctl migrate --experimental-native --undo storage first")
)

var ErrNoNativeEquivalent = errors.New("migrate: a setting has no equivalent without Docker")

const LiveKitEnabledVar = "FLUXER_LIVEKIT_ENABLED"

type NativeTarget interface {
	Dir() string
	Preflight(ctx context.Context, chosen map[string]string) error
	Install(ctx context.Context) error
	Restore(ctx context.Context, archive string) error
	Open(ctx context.Context, chosen map[string]string) (Host, error)
	Discard(ctx context.Context) error
}

var composeDefaults = map[string]string{
	settings.RustProcess:    settings.RustPerService,
	settings.StorageBackend: settings.StorageS3,
	settings.KVMode:         settings.KVStandalone,
	LiveKitEnabledVar:       "true",
	StorageReadOnlyVar:      "",
}

type NativeSettings struct {
	Settings map[string]string
	Left     []string
}

func ForNative(compose map[string]string) (NativeSettings, error) {
	rest := maps.Clone(compose)
	for k := range composeDefaults {
		delete(rest, k)
	}
	kept, left, refused := settings.ForNative(stack.Embedded(), rest)
	for k, def := range composeDefaults {
		v := strings.TrimSpace(compose[k])
		if v == "" {
			v = def
		}
		kept[k] = v
	}
	out := NativeSettings{Settings: kept, Left: left}
	if len(refused) > 0 {
		return out, fmt.Errorf("%w: Fluxer without Docker doesn't use %s. Remove them with rexctl config unset %s first", ErrNoNativeEquivalent, strings.Join(refused, ", "), strings.Join(refused, " "))
	}
	return out, nil
}

type DockerStep struct {
	target    NativeTarget
	recorded  State
	installed bool
}

func NewDockerStep(target NativeTarget, recorded State) *DockerStep {
	return &DockerStep{target: target, recorded: recorded}
}

func (s *DockerStep) Option() Option    { return OptDocker }
func (s *DockerStep) Writers() []string { return nil }

func (s *DockerStep) Preflight(ctx context.Context, r *Run) error {
	if r.Host.Runtime() != Compose {
		return ErrAlreadyNative
	}
	if st := s.recorded.Storage; st != nil && !st.Open() {
		return ErrStorageReadOnly
	}
	c, err := ForNative(r.Host.Settings())
	if err != nil {
		return err
	}
	if len(c.Left) > 0 {
		r.note("These settings only change Docker containers and stay with the Docker instance: %s.", strings.Join(c.Left, ", "))
	}
	return s.target.Preflight(ctx, c.Settings)
}

func (s *DockerStep) Migrate(ctx context.Context, r *Run) error {
	if err := r.Host.Halt(ctx); err != nil {
		return err
	}
	archive, err := r.Host.Backup(ctx)
	if err != nil {
		return err
	}
	s.installed = true
	if err := s.target.Install(ctx); err != nil {
		return err
	}
	return s.target.Restore(ctx, archive)
}

func (s *DockerStep) Verify(context.Context, *Run) error { return nil }

func (s *DockerStep) Switch(ctx context.Context, r *Run) error {
	c, err := ForNative(r.Host.Settings())
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
	r.Host = next
	return nil
}

func (s *DockerStep) Check(context.Context, *Run) error { return nil }

func (s *DockerStep) Rollback(ctx context.Context, _ *Run) error {
	if !s.installed {
		return nil
	}
	return s.target.Discard(ctx)
}
