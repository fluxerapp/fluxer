// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"io"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type Kind string

const (
	Compose   Kind = "compose"
	Native    Kind = "native"
	Container Kind = "container"
)

var ErrUnsupported = errors.New("runtime: not supported")

type Connect func(ctx context.Context) (*engine.Engine, error)

func KindOf(inst *instance.Instance) Kind {
	switch {
	case inst != nil && inst.State.IsNative():
		return Native
	case inst != nil && inst.State.IsContainer():
		return Container
	}
	return Compose
}

func Open(ctx context.Context, inst *instance.Instance, u *ui.UI) (Runtime, error) {
	return OpenWith(ctx, inst, u, nil)
}

func OpenWith(_ context.Context, inst *instance.Instance, u *ui.UI, connect Connect) (Runtime, error) {
	if inst == nil || inst.State == nil {
		return nil, ui.Refused("rexctl can't read this instance's state.", "Run rexctl doctor to see what's wrong.")
	}
	switch inst.State.Runtime {
	case instance.RuntimeNative:
		return newNative(inst, u), nil
	case instance.RuntimeContainer:
		if inst.State.IsVoice() {
			return newVoiceContainer(inst, u, connect), nil
		}
		return newContainer(inst, u, connect), nil
	case "", instance.RuntimeCompose:
		return newCompose(inst, u, connect), nil
	}
	return nil, &ui.ExitError{
		Code: ui.ExitRefused,
		Msg:  "This instance runs on " + inst.State.Runtime + ", which this rexctl doesn't know.",
		Fix:  "Download the latest rexctl, then try again.",
		Err:  ErrUnsupported,
	}
}

type Runtime interface {
	Kind() Kind
	Info(ctx context.Context) Info
	Status(ctx context.Context) (Status, error)

	Start(ctx context.Context, s *ui.Step, o StartOptions) error
	Stop(ctx context.Context, s *ui.Step) error
	Restart(ctx context.Context, s *ui.Step, parts []string) error
	Down(ctx context.Context, s *ui.Step) error
	Logs(ctx context.Context, w io.Writer, o LogOptions) error
	Uninstall(ctx context.Context, s *ui.Step, o UninstallOptions) error

	Boot() Boot
	Admin(ctx context.Context) (adminapi.Transport, error)
	Settings() Settings
	Backups() Backups
	Upgrades() Upgrades
	Voice() Voice
}

type Info struct {
	Kind     Kind
	Label    string
	Unit     string
	Root     bool
	DataDir  string
	EdgeBind string
}

type Part struct {
	Name     string
	Members  []string
	State    PartState
	Health   string
	Since    time.Time
	Restarts int
	CPU      float64
	MemBytes int64
	Optional bool
}

type PartState string

const (
	PartRunning  PartState = "running"
	PartStarting PartState = "starting"
	PartDegraded PartState = "degraded"
	PartStopped  PartState = "stopped"
	PartFailed   PartState = "failed"
	PartAbsent   PartState = "absent"
)

type Status struct {
	Running        bool
	Busy           string
	Parts          []Part
	Release        string
	RunningRelease string
	Restarts       int
	Unit           string
	StartedAt      time.Time
}

type StartOptions struct {
	Boot   bool
	NoWait bool
}

type LogOptions struct {
	Parts  []string
	Follow bool
	Since  string
	Tail   int
}

type UninstallOptions struct {
	KeepData    bool
	KeepBackups bool
	RemoveUser  bool
}

type Boot interface {
	State(ctx context.Context) (BootState, error)
	Enable(ctx context.Context, s *ui.Step) error
	Disable(ctx context.Context, s *ui.Step) error
}

type BootState struct {
	Enabled bool
	Unit    string
	Detail  string
}

type Settings interface {
	Supports(key string) (ok bool, reason string)
	Validate(ctx context.Context, p *deploy.Prepared) error
	Preview(ctx context.Context, p *deploy.Prepared) (Preview, error)
	Apply(ctx context.Context, s *ui.Step, p *deploy.Prepared) (*deploy.Change, error)
	Render(ctx context.Context, w io.Writer) error
}

type Preview struct {
	Restarts []string
	Notes    []string
}

type Backups interface {
	Plan(ctx context.Context, o BackupOptions) (BackupPlan, error)
	Create(ctx context.Context, s *ui.Step, o BackupOptions) (string, error)
	Verify(ctx context.Context, archive string) (*backup.Manifest, error)
	Restore(ctx context.Context, s *ui.Step, archive string) error
}

type BackupOptions struct {
	WithoutUploads bool
	Reason         string
	Dir            string
}

type BackupPlan struct {
	NeedBytes  int64
	FreeBytes  int64
	PauseParts []string
	PauseFor   time.Duration
}

type Upgrades interface {
	Current(ctx context.Context) (string, error)
	Check(ctx context.Context, to *release.Release) error
	Prepare(ctx context.Context, s *ui.Step, to *release.Release) error
	Switch(ctx context.Context, s *ui.Step, to *release.Release) error
	StartVerify(ctx context.Context, s *ui.Step) error
	GoBack(ctx context.Context, s *ui.Step, from string) error
	Prune(ctx context.Context, keep []string) error
}

type Voice interface {
	Local(ctx context.Context) ([]instance.VoiceServer, error)
	Add(ctx context.Context, s *ui.Step, v instance.VoiceServer) error
	Remove(ctx context.Context, s *ui.Step, id string) error
	StartAll(ctx context.Context, s *ui.Step) error
	StopAll(ctx context.Context, s *ui.Step) error
}
