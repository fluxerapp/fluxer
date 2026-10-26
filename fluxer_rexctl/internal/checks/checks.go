// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"sort"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

type Status string

const (
	Pass Status = "pass"
	Warn Status = "warn"
	Fail Status = "fail"
	Skip Status = "skip"
)

type Phase int

const (
	Preinstall Phase = 1 << iota
	Installed
)

type Result struct {
	ID      string   `json:"id"`
	Status  Status   `json:"status"`
	Message string   `json:"message"`
	Fix     string   `json:"fix,omitempty"`
	Fixable bool     `json:"fixable"`
	Fixed   bool     `json:"fixed"`
	Manual  string   `json:"-"`
	Details []string `json:"-"`
}

type Env struct {
	Dir      string
	Domain   string
	Instance *instance.Instance
	Engine   *engine.Engine
	Project  *engine.Project
	Host     *host.Info
	Behind   bool
	Bind     string
	Admin    *adminapi.Client
	Settings map[string]string

	phase         Phase
	prepared      bool
	engineErr     error
	loadErr       error
	env           map[string]string
	staged        map[string]*string
	stagedSecrets map[string]string
	stagedAdmin   map[string]string
	importEnv     bool
	stagedBy      []string
	keptEdits     [][2]string
	publicIP      string
	publicErr     error
	publicDone    bool
	ps            []engine.Container
	psErr         error
	psDone        bool
	pub           map[host.Port]string
	pubErr        error
	pubDone       bool
}

type Check struct {
	ID      string
	Phases  Phase
	Run     func(ctx context.Context, env *Env) Result
	Fix     func(ctx context.Context, env *Env) error
	SafeFix bool
}

var order = []string{
	"engine",
	"podman-subids",
	"podman-setup",
	"windows-linux-containers",
	"memory",
	"disk",
	"ports",
	"low-ports",
	"dns",
	"env-file",
	"stack-files",
	"stack-edits",
	"pending-change",
	"upgrade-unfinished",
	"boot-unit",
	"origin",
	"vapid",
	"services",
	"stale-containers",
	"health",
	"certificate",
	"discovery",
	"app-proxy-discovery",
	"postgres-password",
	"postgres-scram",
	"livekit-ip",
	"client-ip",
	"backup-age",
}

var registry = map[string]Check{}

func Register(c Check) {
	if _, dup := registry[c.ID]; dup {
		panic("checks: duplicate id " + c.ID)
	}
	registry[c.ID] = c
}

func rank(id string) int {
	for i, o := range order {
		if o == id {
			return i
		}
	}
	return len(order)
}

func All() []Check {
	out := make([]Check, 0, len(registry))
	for _, c := range registry {
		out = append(out, c)
	}
	sort.Slice(out, func(i, j int) bool {
		ri, rj := rank(out[i].ID), rank(out[j].ID)
		if ri != rj {
			return ri < rj
		}
		return out[i].ID < out[j].ID
	})
	return out
}

func Get(id string) (Check, bool) {
	c, ok := registry[id]
	return c, ok
}

func Run(ctx context.Context, env *Env, phase Phase) []Result {
	env.phase = phase
	env.prepare(ctx)
	env.reset()
	out := []Result{}
	for _, c := range All() {
		if c.Phases&phase == 0 {
			continue
		}
		out = append(out, RunOne(ctx, env, c))
	}
	return out
}

func RunOne(ctx context.Context, env *Env, c Check) Result {
	if env.phase == 0 {
		env.phase = c.Phases
	}
	env.prepare(ctx)
	r := c.Run(ctx, env)
	r.ID = c.ID
	if env.phase&Installed == 0 {
		r.Fix = strings.Replace(r.Fix, "then run rexctl doctor.", "then run rexctl install.", 1)
	}
	r.Fixable = r.Fixable && c.Fix != nil && (r.Status == Warn || r.Status == Fail)
	return r
}

func Recheck(ctx context.Context, env *Env, c Check) Result {
	env.reset()
	return RunOne(ctx, env, c)
}

func RunIn(ctx context.Context, env *Env, phase Phase, c Check) Result {
	env.phase = phase
	return RunOne(ctx, env, c)
}

func ApplyFix(ctx context.Context, env *Env, id string) (Result, error) {
	c, ok := registry[id]
	if !ok || c.Fix == nil {
		return Result{}, ErrNoFix
	}
	before := env.stagedCount()
	if err := c.Fix(ctx, env); err != nil {
		return Result{}, err
	}
	if env.stagedCount() != before {
		env.stagedBy = append(env.stagedBy, id)
	}
	env.reset()
	r := RunOne(ctx, env, c)
	r.Fixable = true
	r.Fixed = r.Status == Pass || r.Status == Skip
	return r, nil
}

func Failed(results []Result) bool {
	for _, r := range results {
		if r.Status == Fail {
			return true
		}
	}
	return false
}

func AllGood(results []Result) bool {
	for _, r := range results {
		if r.Status == Fail || r.Status == Warn {
			return false
		}
	}
	return true
}
