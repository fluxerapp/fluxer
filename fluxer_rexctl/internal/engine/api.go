// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"io"
	"strings"
	"time"

	"github.com/compose-spec/compose-go/v2/types"
)

type Kind string

const (
	Docker Kind = "docker"
	Podman Kind = "podman"
)

const (
	MinPodman = "5.4.0"
	MinDocker = "24.0.0"
)

const DefaultReadyTimeout = 10 * time.Minute

type Engine struct {
	Kind       Kind
	Host       string
	ID         string
	Name       string
	Version    string
	APIVersion string
	OSType     string
	Rootless   bool
	Desktop    string
	DataRoot   string
	TooOld     bool
}

type ConnectOptions struct {
	Host     string
	ID       string
	Prefer   Kind
	AllowOld bool
}

type Proxies struct {
	HTTP, HTTPS, No string
}

type Spec struct {
	Name  string
	Dir   string
	Files []string
	Env   map[string]string
}

type Project struct {
	Engine *Engine
	Name   string
	Dir    string
	Model  *types.Project
}

type Plan struct {
	Create    []string
	Recreate  []string
	Remove    []string
	Networks  []string
	LabelOnly []string
	Volumes   []string
}

type UpOptions struct {
	Services      []string
	NoDeps        bool
	Recreate      Recreate
	RemoveOrphans bool
	Wait          bool
	WaitTimeout   time.Duration
	OnEvent       func(Event)
}

type Recreate int

const (
	RecreateChanged Recreate = iota
	RecreateAlways
	RecreateNever
)

type ExecOptions struct {
	User         string
	Env          []string
	Stdin        io.Reader
	Stdout       io.Writer
	Stderr       io.Writer
	KillOnCancel bool
}

type HelperOptions struct {
	Image      string
	Entrypoint []string
	Command    []string
	Mounts     []Mount
	Stdin      io.Reader
	Stdout     io.Writer
	Stderr     io.Writer
}

type Mount struct {
	Volume   string
	Bind     string
	Target   string
	ReadOnly bool
}

type EventStatus string

const (
	EventWorking EventStatus = "working"
	EventDone    EventStatus = "done"
	EventWarning EventStatus = "warning"
	EventError   EventStatus = "error"
)

type Event struct {
	ID      string
	Parent  string
	Kind    string
	Service string
	Text    string
	Status  EventStatus
	Current int64
	Total   int64
}

type Container struct {
	ID, Name, Service, State, Health, Image, ImageID string
	Number, ExitCode, RestartCount                   int
	Labels                                           map[string]string
	Created                                          time.Time
}

type Volume struct {
	Name       string
	Project    string
	Labels     map[string]string
	Mountpoint string
	Device     string
}

type ProjectInfo struct {
	Name        string
	Status      string
	ConfigFiles string
}

type Readiness struct {
	Ready    []string
	NotReady []string
	Failed   []string
}

type NotReadyError struct {
	Readiness Readiness
	Fatal     bool
	Crashing  []string
}

func (e *NotReadyError) Error() string {
	parts := append(append([]string{}, e.Readiness.Failed...), e.Readiness.NotReady...)
	return "engine: not ready: " + strings.Join(parts, ", ")
}

type LogsOptions struct {
	Follow bool
	Since  string
	Tail   int
	Stdout io.Writer
}

type Error struct {
	Op       string
	Class    Class
	Images   []string
	Other    string
	Replaced bool
	Err      error
}

func (e *Error) Error() string {
	if e.Err == nil {
		return "engine: " + e.Op
	}
	return "engine: " + e.Op + ": " + e.Err.Error()
}

func (e *Error) Unwrap() error { return e.Err }

type Class int

const (
	ClassOther Class = iota
	ClassUnreachable
	ClassDenied
	ClassWindowsContainers
	ClassPodmanTooOld
	ClassEngineChanged
	ClassRateLimit
	ClassCredentials
	ClassAuth
	ClassImageNotFound
	ClassDNS
	ClassTLS
	ClassProxy
	ClassTimeout
	ClassDiskFull
	ClassPortInUse
	ClassAddressPools
	ClassSubIDs
	ClassDependencyFailed
	ClassVolumeChanged
	ClassInvalidConfig
	ClassForeignProject
)
