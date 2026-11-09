// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	KVURLVar           = "FLUXER_KV_URL"
	DatabaseBackendVar = "FLUXER_DATABASE_BACKEND"
	ServerMaxConnsVar  = "FLUXER_POSTGRES_SERVER_MAX_CONNECTIONS"
	ValkeyService      = "valkey"
)

var (
	kvWriters = []string{"api", "worker", "gateway"}

	ErrKVCluster   = errors.New("migrate: a Redis-compatible cluster isn't moved by this change")
	ErrKVDatabase  = errors.New("migrate: Postgres KV needs the postgres database backend")
	ErrConnections = errors.New("migrate: Postgres allows too few connections")
	ErrCleanedUp   = errors.New("migrate: the old data is already removed")
)

type kvDigest struct {
	Kind string `json:"kind"`
	Keys int    `json:"keys"`
}

type kvReport struct {
	Verified      bool       `json:"verified"`
	Copied        int        `json:"copied"`
	Kinds         []kvDigest `json:"kinds"`
	TargetKinds   []kvDigest `json:"targetKinds"`
	Skipped       int        `json:"skipped"`
	TTLMismatches []string   `json:"ttlMismatches"`
}

func keys(d []kvDigest) int {
	n := 0
	for _, k := range d {
		n += k.Keys
	}
	return n
}

func (r kvReport) problems() string {
	var out []string
	src := map[string]int{}
	for _, k := range r.Kinds {
		src[k.Kind] = k.Keys
	}
	for _, k := range r.TargetKinds {
		if src[k.Kind] != k.Keys {
			out = append(out, fmt.Sprintf("%s: %d keys in the source, %d in the target", k.Kind, src[k.Kind], k.Keys))
		}
	}
	if n := len(r.TTLMismatches); n > 0 {
		out = append(out, fmt.Sprintf("%d keys expire at different times, such as %s", n, r.TTLMismatches[0]))
	}
	if len(out) == 0 {
		out = append(out, "the sha256 of a kind differs")
	}
	return strings.Join(out, ", ")
}

func ConnectionBudget(env map[string]string) (need, limit int, ok bool) {
	limit, err := strconv.Atoi(strings.TrimSpace(env[ServerMaxConnsVar]))
	if err != nil || limit <= 0 {
		return 0, 0, false
	}
	num := func(k string, fallback int) int {
		if n, err := strconv.Atoi(strings.TrimSpace(env[k])); err == nil && n > 0 {
			return n
		}
		return fallback
	}
	need = num("FLUXER_API_POSTGRES_MAX_CONNECTIONS", 25) +
		num("FLUXER_USERS_POSTGRES_MAX_CONNECTIONS", 20) + num("FLUXER_MESSAGES_POSTGRES_MAX_CONNECTIONS", 20)
	workers := 0
	if !strings.EqualFold(strings.TrimSpace(env[stack.EmbeddedWorkerVar]), "true") {
		workers = num(stack.WorkerReplicasVar, 1)
		need += workers * num("FLUXER_WORKER_POSTGRES_MAX_CONNECTIONS", 25)
	}
	need += (1 + workers) * (num(stack.KVPoolVar, 25) + 1)
	return need, limit, true
}

func kvChanges(current map[string]string) map[string]string {
	out := map[string]string{settings.KVMode: settings.KVPostgres}
	if strings.TrimSpace(current[KVURLVar]) != "" {
		out[KVURLVar] = ""
	}
	return out
}

func kvJob(r *Run, args ...string) Job {
	return Job{Args: append([]string{ImportKV}, args...), Stderr: r.Log}
}

type KVStep struct {
	host     Host
	prev     map[string]string
	switched bool
	retired  string
}

func NewKVStep() *KVStep { return &KVStep{} }

func (s *KVStep) Option() Option    { return OptKV }
func (s *KVStep) Writers() []string { return kvWriters }

func (s *KVStep) Preflight(ctx context.Context, r *Run) error {
	h := r.Host
	current := h.Settings()
	if strings.TrimSpace(current[settings.KVMode]) == settings.KVCluster {
		return ErrKVCluster
	}
	env, err := h.Env(nil)
	if err != nil {
		return err
	}
	if b := strings.TrimSpace(env[DatabaseBackendVar]); b != "" && b != "postgres" {
		return fmt.Errorf("%w, and FLUXER_DATABASE_BACKEND is %s", ErrKVDatabase, b)
	}
	changes := kvChanges(current)
	target, err := h.Env(changes)
	if err != nil {
		return err
	}
	if need, limit, ok := ConnectionBudget(target); ok && need > limit && h.Runtime() == Compose {
		return fmt.Errorf("%w: the services and their replicas need %d with the KV pools, and max_connections is %d", ErrConnections, need, limit)
	}
	if err := h.CheckTarget(ctx, changes); err != nil {
		return err
	}
	var rep kvReport
	j := kvJob(r, "--verify-only")
	j.EnvArgs = [][2]string{{"--from-url", KVURLVar}}
	if err := runJSON(ctx, h.Importer(), j, &rep); err != nil {
		return fmt.Errorf("reading Valkey and Postgres: %w", err)
	}
	if n := keys(rep.TargetKinds); n > 0 {
		r.note("Postgres holds %d keys from an earlier move, and rexctl replaces them with what Valkey holds now.", n)
	}
	r.note("Valkey holds %d keys to move, not counting %d rate limit keys.", keys(rep.Kinds), rep.Skipped)
	return nil
}

func (s *KVStep) Migrate(ctx context.Context, r *Run) error {
	s.host = r.Host
	return s.copy(ctx, r, []string{"--replace"})
}

func (s *KVStep) Verify(ctx context.Context, r *Run) error {
	return s.copy(ctx, r, []string{"--verify-only"})
}

func (s *KVStep) copy(ctx context.Context, r *Run, extra []string) error {
	var rep kvReport
	j := kvJob(r, extra...)
	j.EnvArgs = [][2]string{{"--from-url", KVURLVar}}
	if err := runJSON(ctx, r.Host.Importer(), j, &rep); err != nil {
		return err
	}
	if !rep.Verified {
		return fmt.Errorf("the copy in Postgres doesn't match Valkey: %s", rep.problems())
	}
	return nil
}

func (s *KVStep) Switch(ctx context.Context, r *Run) error {
	h := r.Host
	current := h.Settings()
	changes := kvChanges(current)
	s.prev = previous(current, changes)
	if err := h.Apply(ctx, changes); err != nil {
		return err
	}
	s.switched = true
	at := r.now()
	dir, err := h.RetireValkey(ctx, at.Format("20060102-150405"))
	if err != nil {
		return err
	}
	s.retired = dir
	r.State.KV = &Switched{At: at, Previous: s.prev, ValkeyDir: dir}
	return nil
}

func (s *KVStep) Check(context.Context, *Run) error { return nil }

func (s *KVStep) Rollback(ctx context.Context, _ *Run) error {
	if s.host == nil || !s.switched {
		return nil
	}
	if s.retired != "" {
		if err := s.host.ReviveValkey(ctx, s.retired); err != nil {
			return err
		}
	}
	return s.host.Apply(ctx, s.prev)
}

func UndoKV(ctx context.Context, h Host, st *State, r *Run) error {
	switch {
	case st.KV == nil:
		return ErrNotSwitched
	case st.KV.CleanedUpAt != nil:
		return ErrCleanedUp
	}
	if err := h.Stop(ctx, kvWriters); err != nil {
		return err
	}
	if st.KV.ValkeyDir != "" {
		if err := h.ReviveValkey(ctx, st.KV.ValkeyDir); err != nil {
			return err
		}
	}
	if err := copyBack(ctx, h, st, r); err != nil {
		return errors.Join(err, stayOnPostgres(ctx, h, st, r))
	}
	if err := h.Start(ctx); err != nil {
		return err
	}
	if err := h.Check(ctx); err != nil {
		return err
	}
	st.KV = nil
	return nil
}

func copyBack(ctx context.Context, h Host, st *State, r *Run) error {
	if err := h.Apply(ctx, st.KV.Previous); err != nil {
		return err
	}
	if err := h.StartOnly(ctx, []string{ValkeyService}); err != nil {
		return err
	}
	if err := h.Stop(ctx, kvWriters); err != nil {
		return err
	}
	for _, extra := range [][]string{{"--replace"}, {"--verify-only"}} {
		var rep kvReport
		j := kvJob(r, append([]string{"--to", "valkey"}, extra...)...)
		j.EnvArgs = [][2]string{{"--to-url", KVURLVar}}
		if err := runJSON(ctx, h.Importer(), j, &rep); err != nil {
			return err
		}
		if !rep.Verified {
			return fmt.Errorf("the copy in Valkey doesn't match Postgres: %s", rep.problems())
		}
	}
	return nil
}

func stayOnPostgres(ctx context.Context, h Host, st *State, r *Run) error {
	ctx = context.WithoutCancel(ctx)
	if err := h.Stop(ctx, kvWriters); err != nil {
		return err
	}
	if err := h.Apply(ctx, kvChanges(h.Settings())); err != nil {
		return err
	}
	if st.KV.ValkeyDir != "" {
		dir, err := h.RetireValkey(ctx, r.now().Format("20060102-150405"))
		if err != nil {
			return err
		}
		st.KV.ValkeyDir = dir
	}
	return h.Start(ctx)
}
