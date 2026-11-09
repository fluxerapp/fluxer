// SPDX-License-Identifier: AGPL-3.0-or-later

package spec

import (
	"context"
	"net"
	"path/filepath"
	"slices"
	"strconv"
	"syscall"
	"time"
)

const (
	Postgres       = "postgres"
	Valkey         = "valkey"
	SeaweedFS      = "seaweedfs"
	SeaweedFSFiler = "seaweedfs-filer"
	SeaweedFSInit  = "seaweedfs-init"
	Meilisearch    = "meilisearch"
	API            = "api"
	Gateway        = "gateway"
	MediaProxy     = "media-proxy"
	Admin          = "admin"
	AppProxy       = "app-proxy"
	Push           = "push"
	Snowflakes     = "snowflakes"
	Users          = "users"
	Messages       = "messages"
	Gifs           = "gifs"
	Unfurl         = "unfurl"
	LiveKit        = "livekit"
	RustServer     = "rust"
)

var RustServices = []string{Snowflakes, Users, Messages, Gifs, Unfurl, Push, AppProxy, Admin}

const Loopback = "127.0.0.1"

const (
	PortNATS          = 14222
	PortPostgres      = 15432
	PortValkey        = 16379
	PortMeilisearch   = 17700
	PortLiveKit       = 17880
	PortAPI           = 18080
	PortGateway       = 18081
	PortMediaProxy    = 18082
	PortAdmin         = 18083
	PortAppProxy      = 18084
	PortPush          = 18085
	PortSnowflakes    = 18090
	PortUsers         = 18091
	PortMessages      = 18092
	PortGifs          = 18093
	PortUnfurl        = 18094
	PortSeaweedS3     = 18333
	PortWeedMaster    = 19333
	PortWeedVolume    = 19334
	PortWeedFiler     = 19335
	PortLiveKitRTCTCP = 7881
	PortLiveKitRTCUDP = 7882
	PortEdgeProxy     = 8080
)

func Addr(port int) string {
	return net.JoinHostPort(Loopback, strconv.Itoa(port))
}

func URL(port int) string {
	return "http://" + Addr(port)
}

const (
	DataDirEnv        = "FLUXER_DATA_DIR"
	RuntimeDirEnv     = "RUNTIME_DIRECTORY"
	DefaultDataDir    = "/var/lib/fluxer"
	DefaultRuntimeDir = "/run/fluxer"
	ConfigDir         = "/etc/fluxer"
	DefaultEnvFile    = "/etc/fluxer/fluxer.env"
	DefaultCaddyDir   = "/etc/fluxer/caddy.d"
	DefaultCertsDir   = "/etc/fluxer/certs"
)

const (
	WorkerHeartbeatPath   = "/tmp/fluxer-worker-heartbeat"
	WorkerHeartbeatMaxAge = 30 * time.Second
)

type Layout struct {
	Data    string
	Runtime string
}

func LayoutFromEnv(getenv func(string) string) Layout {
	l := Layout{Data: getenv(DataDirEnv), Runtime: getenv(RuntimeDirEnv)}
	if l.Data == "" {
		l.Data = DefaultDataDir
	}
	if l.Runtime == "" {
		l.Runtime = DefaultRuntimeDir
	}
	return l
}

func (l Layout) Dir(svc string) string     { return filepath.Join(l.Data, svc) }
func (l Layout) PostgresDir() string       { return filepath.Join(l.Data, "postgres") }
func (l Layout) ValkeyDir() string         { return filepath.Join(l.Data, "valkey") }
func (l Layout) NATSDir() string           { return filepath.Join(l.Data, "nats") }
func (l Layout) SeaweedFSDir() string      { return filepath.Join(l.Data, "seaweedfs") }
func (l Layout) MeilisearchDir() string    { return filepath.Join(l.Data, "meilisearch") }
func (l Layout) EdgeDir() string           { return filepath.Join(l.Data, "edge") }
func (l Layout) GatewayHome() string       { return filepath.Join(l.Data, "gateway") }
func (l Layout) TmpRoot() string           { return filepath.Join(l.Data, "tmp") }
func (l Layout) Tmp(svc string) string     { return filepath.Join(l.Data, "tmp", svc) }
func (l Layout) BackupsDir() string        { return filepath.Join(l.Data, "backups") }
func (l Layout) SecretsFile() string       { return filepath.Join(l.Data, "secrets.env") }
func (l Layout) ModesFile() string         { return filepath.Join(l.Data, "modes.env") }
func (l Layout) LockFile() string          { return filepath.Join(l.Data, ".fluxerd.lock") }
func (l Layout) ControlSocket() string     { return filepath.Join(l.Runtime, "fluxerd.sock") }
func (l Layout) OperatorSocket() string    { return filepath.Join(l.Runtime, "operator.sock") }
func (l Layout) PGSocketDir() string       { return filepath.Join(l.Runtime, "pg") }
func (l Layout) GatewayRuntimeDir() string { return filepath.Join(l.Runtime, "gateway") }

type Env map[string]string

func (e Env) List() []string {
	out := make([]string, 0, len(e))
	for k, v := range e {
		out = append(out, k+"="+v)
	}
	slices.Sort(out)
	return out
}

type ProbeKind string

const (
	ProbeHTTP      ProbeKind = "http"
	ProbeExec      ProbeKind = "exec"
	ProbeFileFresh ProbeKind = "file-fresh"
	ProbeFunc      ProbeKind = "func"
)

type Probe struct {
	Name   string
	Kind   ProbeKind
	Addr   string
	Path   string
	Status int
	Argv   []string
	File   string
	MaxAge time.Duration
	Func   func(context.Context) error
}

func HTTPProbe(port int, path string, status int) Probe {
	return Probe{Kind: ProbeHTTP, Addr: Addr(port), Path: path, Status: status}
}

func ExecProbe(argv ...string) Probe {
	return Probe{Kind: ProbeExec, Argv: argv}
}

func FileFreshProbe(path string, maxAge time.Duration) Probe {
	return Probe{Kind: ProbeFileFresh, File: path, MaxAge: maxAge}
}

func FuncProbe(fn func(context.Context) error) Probe {
	return Probe{Kind: ProbeFunc, Func: fn}
}

type Child struct {
	Name        string
	Argv        []string
	Env         Env
	Cwd         string
	Deps        []string
	Probes      []Probe
	Watch       []Probe
	SvcLogs     bool
	StopSignal  syscall.Signal
	StopTimeout time.Duration
	Core        bool
	Oneshot     bool
}

type State string

const (
	StateWaiting    State = "waiting"
	StateStarting   State = "starting"
	StateRunning    State = "running"
	StateRestarting State = "restarting"
	StateCrashloop  State = "crashloop"
	StateStopping   State = "stopping"
	StateStopped    State = "stopped"
	StateExited     State = "exited"
)

type HealthState string

const (
	HealthNone      HealthState = ""
	HealthStarting  HealthState = "starting"
	HealthHealthy   HealthState = "healthy"
	HealthUnhealthy HealthState = "unhealthy"
)

type PsEntry struct {
	Name      string       `json:"name"`
	State     State        `json:"state"`
	Health    HealthState  `json:"health"`
	Restarts  int          `json:"restarts"`
	PID       int          `json:"pid"`
	StartedAt time.Time    `json:"started_at"`
	ExitCode  int          `json:"exit_code"`
	Core      bool         `json:"core"`
	Oneshot   bool         `json:"oneshot"`
	Watch     []WatchEntry `json:"watch,omitempty"`
}

type WatchEntry struct {
	Name   string      `json:"name"`
	Health HealthState `json:"health"`
	Error  string      `json:"error,omitempty"`
}

type Health struct {
	Ready    bool     `json:"ready"`
	Stopping bool     `json:"stopping"`
	Pending  []string `json:"pending"`
	Failed   []string `json:"failed"`
	Degraded []string `json:"degraded,omitempty"`
}

const (
	PathPs        = "/v1/ps"
	PathHealth    = "/v1/health"
	PathSvc       = "/v1/svc/"
	PathLogs      = "/v1/logs/"
	PathJetStream = "/v1/jetstream/"
)

const (
	ActionStart   = "start"
	ActionStop    = "stop"
	ActionRestart = "restart"
)

const ExitCrashloop = 70
