// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"runtime"
	"slices"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/docker/cli/cli/command"
	"github.com/docker/cli/cli/flags"
	"github.com/docker/compose/v5/pkg/api"
	"github.com/docker/compose/v5/pkg/compose"
	"github.com/moby/moby/client"
	"github.com/sirupsen/logrus"
)

const probeTimeout = 5 * time.Second

type conn struct {
	cli     *command.DockerCli
	svc     api.Compose
	bus     *bus
	proxies Proxies
}

var (
	connsMu sync.Mutex
	conns   = map[*Engine]*conn{}

	detailMu sync.Mutex
	detail   io.Writer = io.Discard
)

func SetDetail(w io.Writer) {
	if w == nil {
		w = io.Discard
	}
	detailMu.Lock()
	detail = w
	detailMu.Unlock()
	logrus.SetOutput(w)
}

func detailf(format string, args ...any) {
	detailMu.Lock()
	defer detailMu.Unlock()
	fmt.Fprintf(detail, format+"\n", args...)
}

func (e *Engine) conn() (*conn, error) {
	connsMu.Lock()
	defer connsMu.Unlock()
	c, ok := conns[e]
	if !ok {
		return nil, &Error{Op: "connect", Class: ClassUnreachable, Err: errors.New("the engine is not connected")}
	}
	return c, nil
}

func (e *Engine) api() (client.APIClient, error) {
	c, err := e.conn()
	if err != nil {
		return nil, err
	}
	return c.cli.Client(), nil
}

func (k Kind) Name() string {
	if k == Podman {
		return "Podman"
	}
	return "Docker"
}

func (k Kind) InstallHint() string {
	switch {
	case k == Podman:
		return "Install Podman"
	case runtime.GOOS == "linux":
		return "Install Docker from https://docs.docker.com/engine/install/"
	}
	return "Install Docker Desktop from https://docs.docker.com/desktop/"
}

func Connect(ctx context.Context, opts ConnectOptions) (*Engine, error) {
	logrus.SetOutput(currentDetail())
	logrus.SetLevel(logrus.InfoLevel)
	if opts.Host != "" {
		e, c, err := dial(ctx, opts.Host)
		if err == nil {
			if opts.ID != "" && e.ID != opts.ID {
				c.close()
				return nil, &Error{Op: "connect", Class: ClassEngineChanged, Other: e.DisplayName(), Replaced: true, Err: fmt.Errorf("%s answers as engine %s, not the engine %s this instance uses", opts.Host, e.ID, opts.ID)}
			}
			return finish(e, c, opts)
		}
		if opts.ID == "" {
			return nil, err
		}
		detailf("engine: %s did not answer, looking for engine %s elsewhere", opts.Host, opts.ID)
		e, c, found, other := findByID(ctx, opts)
		if found {
			return finish(e, c, opts)
		}
		if other != nil {
			return nil, &Error{Op: "connect", Class: ClassEngineChanged, Other: other.DisplayName(), Err: fmt.Errorf("the engine %s at %s does not answer, and %s at %s is another engine", opts.ID, opts.Host, other.ID, other.Host)}
		}
		return nil, &Error{Op: "connect", Class: classOrUnreachable(err), Err: fmt.Errorf("the engine %s at %s does not answer: %w", opts.ID, opts.Host, err)}
	}
	var firstErr error
	for _, host := range candidates(opts.Prefer) {
		e, c, err := dial(ctx, host)
		if err == nil {
			return finish(e, c, opts)
		}
		detailf("engine: %s: %v", host, err)
		if rootOnly(host, err) {
			err = &Error{Op: "connect", Class: ClassUnreachable, Err: err}
		}
		if firstErr == nil || (ClassOf(firstErr) != ClassDenied && ClassOf(err) == ClassDenied) {
			firstErr = err
		}
	}
	if firstErr == nil {
		firstErr = &Error{Op: "connect", Class: ClassUnreachable, Err: errors.New("no engine answers")}
	}
	return nil, firstErr
}

const rootPodman = "unix:///run/podman/podman.sock"

func rootOnly(host string, err error) bool {
	return host == rootPodman && os.Geteuid() > 0 && os.Getenv("DOCKER_HOST") != host && ClassOf(err) == ClassDenied
}

func currentDetail() io.Writer {
	detailMu.Lock()
	defer detailMu.Unlock()
	return detail
}

func findByID(ctx context.Context, opts ConnectOptions) (*Engine, *conn, bool, *Engine) {
	var other *Engine
	for _, host := range candidates(opts.Prefer) {
		if host == opts.Host {
			continue
		}
		e, c, err := dial(ctx, host)
		if err != nil {
			continue
		}
		if e.ID == opts.ID {
			detailf("engine: found engine %s at %s", opts.ID, host)
			return e, c, true, nil
		}
		if other == nil {
			other = e
		}
		c.close()
	}
	return nil, nil, false, other
}

func (e *Engine) DisplayName() string {
	if e.Desktop != "" {
		return e.Desktop
	}
	return e.Kind.Name()
}

func classOrUnreachable(err error) Class {
	if c := ClassOf(err); c == ClassDenied {
		return c
	}
	return ClassUnreachable
}

func candidates(prefer Kind) []string {
	type probe struct {
		host string
		kind Kind
	}
	var list []probe
	home, _ := os.UserHomeDir()
	if runtime.GOOS == "windows" {
		list = append(list, probe{"npipe:////./pipe/docker_engine", Docker}, probe{"npipe:////./pipe/dockerDesktopLinuxEngine", Docker})
	} else {
		if xdg := os.Getenv("XDG_RUNTIME_DIR"); xdg != "" {
			list = append(list,
				probe{"unix://" + filepath.Join(xdg, "podman", "podman.sock"), Podman},
				probe{"unix://" + filepath.Join(xdg, "docker.sock"), Docker})
		}
		list = append(list, probe{rootPodman, Podman}, probe{"unix:///var/run/docker.sock", Docker})
		if home != "" {
			for _, p := range [][]string{{".docker", "run"}, {".orbstack", "run"}, {".colima", "default"}, {".rd"}} {
				list = append(list, probe{"unix://" + filepath.Join(append(append([]string{home}, p...), "docker.sock")...), Docker})
			}
		}
	}
	if prefer != "" {
		slices.SortStableFunc(list, func(a, b probe) int {
			switch {
			case a.kind == prefer && b.kind != prefer:
				return -1
			case a.kind != prefer && b.kind == prefer:
				return 1
			}
			return 0
		})
	}
	out := []string{}
	seen := map[string]bool{}
	add := func(h string) {
		if h != "" && !seen[h] {
			seen[h] = true
			out = append(out, h)
		}
	}
	add(resolveDefault())
	for _, p := range list {
		add(p.host)
	}
	return out
}

func resolveDefault() string {
	cli, err := command.NewDockerCli(command.WithOutputStream(io.Discard), command.WithErrorStream(io.Discard))
	if err != nil {
		return ""
	}
	if err := cli.Initialize(flags.NewClientOptions()); err != nil {
		detailf("engine: docker context: %v", err)
		return ""
	}
	return cli.DockerEndpoint().Host
}

func socketPath(host string) (string, bool) {
	p, ok := strings.CutPrefix(host, "unix://")
	return p, ok
}

func dial(ctx context.Context, host string) (*Engine, *conn, error) {
	if p, ok := socketPath(host); ok {
		if _, err := os.Stat(p); errors.Is(err, os.ErrNotExist) {
			return nil, nil, &Error{Op: "connect", Class: ClassUnreachable, Err: fmt.Errorf("nothing answers at %s", host)}
		}
	}
	cli, err := command.NewDockerCli(command.WithOutputStream(io.Discard), command.WithErrorStream(io.Discard))
	if err != nil {
		return nil, nil, &Error{Op: "connect", Class: ClassUnreachable, Err: err}
	}
	if err := cli.Initialize(&flags.ClientOptions{Hosts: []string{host}}); err != nil {
		return nil, nil, &Error{Op: "connect", Class: ClassUnreachable, Err: err}
	}
	c := &conn{cli: cli}
	api := cli.Client()
	pctx, cancel := context.WithTimeout(ctx, probeTimeout)
	defer cancel()
	if _, err := api.Ping(pctx, client.PingOptions{NegotiateAPIVersion: true}); err != nil {
		c.close()
		return nil, nil, dialError(host, err)
	}
	info, err := api.Info(pctx, client.InfoOptions{})
	if err != nil {
		c.close()
		return nil, nil, dialError(host, err)
	}
	ver, err := api.ServerVersion(pctx, client.ServerVersionOptions{})
	if err != nil {
		c.close()
		return nil, nil, dialError(host, err)
	}
	e := &Engine{
		Kind:       Docker,
		Host:       host,
		ID:         info.Info.ID,
		Name:       info.Info.Name,
		Version:    ver.Version,
		APIVersion: api.ClientVersion(),
		OSType:     info.Info.OSType,
		Desktop:    desktopName(info.Info.OperatingSystem),
	}
	for _, comp := range ver.Components {
		if comp.Name == "Podman Engine" {
			e.Kind = Podman
			e.Version = comp.Version
		}
	}
	for _, opt := range info.Info.SecurityOptions {
		if strings.Contains(opt, "name=rootless") {
			e.Rootless = true
		}
	}
	if _, local := socketPath(host); local && runtime.GOOS == "linux" && e.Desktop == "" {
		e.DataRoot = info.Info.DockerRootDir
	}
	if e.Kind == Podman {
		sum := sha256.Sum256([]byte(info.Info.Name + "\n" + info.Info.DockerRootDir))
		e.ID = "podman:" + hex.EncodeToString(sum[:12])
	}
	return e, c, nil
}

func dialError(host string, err error) error {
	class := ClassUnreachable
	if ClassOf(err) == ClassDenied {
		class = ClassDenied
	}
	return &Error{Op: "connect", Class: class, Err: fmt.Errorf("%s: %w", host, err)}
}

func desktopName(os string) string {
	for _, name := range []string{"Docker Desktop", "OrbStack", "Rancher Desktop", "Colima"} {
		if strings.Contains(os, name) {
			return name
		}
	}
	return ""
}

func finish(e *Engine, c *conn, opts ConnectOptions) (*Engine, error) {
	if strings.EqualFold(e.OSType, "windows") {
		c.close()
		return nil, &Error{Op: "connect", Class: ClassWindowsContainers, Err: fmt.Errorf("%s runs Windows containers", e.Host)}
	}
	if e.Kind == Podman && !AtLeast(e.Version, MinPodman) {
		if !opts.AllowOld {
			c.close()
			return nil, &Error{Op: "connect", Class: ClassPodmanTooOld, Err: fmt.Errorf("podman %s is older than %s", e.Version, MinPodman)}
		}
		e.TooOld = true
	}
	cfg := c.cli.ConfigFile()
	if cfg != nil {
		p, ok := cfg.Proxies[c.cli.DockerEndpoint().Host]
		if !ok {
			p = cfg.Proxies["default"]
		}
		c.proxies = Proxies{HTTP: p.HTTPProxy, HTTPS: p.HTTPSProxy, No: p.NoProxy}
		cfg.Proxies = nil
	}
	c.bus = &bus{}
	svc, err := compose.NewComposeService(c.cli,
		compose.WithPrompt(keepVolume),
		compose.WithEventProcessor(c.bus),
		compose.WithMaxConcurrency(4),
		compose.WithOutputStream(io.Discard),
		compose.WithErrorStream(io.Discard),
	)
	if err != nil {
		c.close()
		return nil, &Error{Op: "connect", Class: ClassOther, Err: err}
	}
	c.svc = svc
	connsMu.Lock()
	conns[e] = c
	connsMu.Unlock()
	return e, nil
}

func keepVolume(message string, _ bool) (bool, error) {
	detailf("engine: %s Kept the existing volume and its data.", message)
	return false, nil
}

func (c *conn) close() {
	if c.cli != nil {
		_ = c.cli.Client().Close()
	}
}

func (e *Engine) Close() error {
	connsMu.Lock()
	c, ok := conns[e]
	delete(conns, e)
	connsMu.Unlock()
	if ok {
		c.close()
	}
	return nil
}

func (e *Engine) Proxies() Proxies {
	c, err := e.conn()
	if err != nil {
		return Proxies{}
	}
	return c.proxies
}

func (e *Engine) MinVersion() string {
	if e.Kind == Podman {
		return MinPodman
	}
	return MinDocker
}

func (e *Engine) refuseTooOld(op string) error {
	if e.TooOld {
		return &Error{Op: op, Class: ClassPodmanTooOld, Err: fmt.Errorf("podman %s is older than %s", e.Version, MinPodman)}
	}
	return nil
}

var versionPattern = regexp.MustCompile(`v?(\d+\.\d+(?:\.\d+)?)`)

func AtLeast(version, minimum string) bool {
	a, b := versionParts(version), versionParts(minimum)
	for i := range 3 {
		if a[i] != b[i] {
			return a[i] > b[i]
		}
	}
	return true
}

func versionParts(v string) [3]int {
	var out [3]int
	m := versionPattern.FindStringSubmatch(v)
	if m == nil {
		return out
	}
	for i, p := range strings.SplitN(m[1], ".", 3) {
		n, _ := strconv.Atoi(p)
		out[i] = n
	}
	return out
}
