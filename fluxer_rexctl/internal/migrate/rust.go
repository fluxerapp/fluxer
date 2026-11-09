// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

var (
	RustHealthTimeout = 60 * time.Second
	pollEvery         = 2 * time.Second
)

const pageScript = `(async()=>{const u=process.argv[1];await (await fetch(u)).arrayBuffer();` +
	`await new Promise(r=>setTimeout(r,3000));const r=await fetch(u);const b=Buffer.from(await r.arrayBuffer());` +
	`console.log(JSON.stringify({status:r.status,csp:r.headers.get('content-security-policy')||'',` +
	`sha256:require('node:crypto').createHash('sha256').update(b).digest('hex')}))})()` +
	`.catch(e=>{console.error(String(e&&e.message||e));process.exit(1)})`

type page struct {
	Status int    `json:"status"`
	CSP    string `json:"csp"`
	SHA256 string `json:"sha256"`
}

func fetchPage(ctx context.Context, h Host, log Job) (page, error) {
	var p page
	log.Script, log.Args = pageScript, []string{h.AppProxyURL() + "/"}
	err := runJSON(ctx, h.Importer(), log, &p)
	return p, err
}

type RustStep struct {
	host     Host
	prev     map[string]string
	late     bool
	baseline *page
}

func NewRustStep() *RustStep { return &RustStep{} }

var rustOn = map[string]string{settings.RustProcess: settings.RustSingle}

func (s *RustStep) Option() Option    { return OptRust }
func (s *RustStep) Writers() []string { return nil }

func (s *RustStep) Preflight(ctx context.Context, r *Run) error {
	if r.Host.Runtime() == Compose && slices.Contains(r.Picked, OptDocker) {
		s.late = true
		return nil
	}
	if err := r.Host.CheckTarget(ctx, rustOn); err != nil {
		return err
	}
	p, err := fetchPage(ctx, r.Host, Job{Stderr: r.Log})
	if err != nil {
		r.note("rexctl couldn't read the app page before the change (%v), so it doesn't compare the page after it.", err)
		return nil
	}
	s.baseline = &p
	return nil
}

func (s *RustStep) Migrate(context.Context, *Run) error { return nil }
func (s *RustStep) Verify(context.Context, *Run) error  { return nil }

func (s *RustStep) Switch(ctx context.Context, r *Run) error {
	h := r.Host
	s.host = h
	s.prev = previous(h.Settings(), rustOn)
	if s.late {
		if err := h.CheckTarget(ctx, rustOn); err != nil {
			return err
		}
	}
	if err := h.Remove(ctx, stack.RustServices); err != nil {
		return err
	}
	svcs, err := h.Services(ctx)
	if err != nil {
		return err
	}
	var still []string
	for _, svc := range svcs {
		if svc.Running && slices.Contains(stack.RustServices, svc.Name) {
			still = append(still, svc.Name)
		}
	}
	if len(still) > 0 {
		return fmt.Errorf("%s still run after rexctl removed them", strings.Join(still, ", "))
	}
	if err := h.FreePorts(ctx, RustPorts()); err != nil {
		return err
	}
	return h.Apply(ctx, rustOn)
}

func (s *RustStep) Check(ctx context.Context, r *Run) error {
	if err := waitHealthy(ctx, r.Host, stack.RustService, RustHealthTimeout); err != nil {
		return err
	}
	if s.baseline == nil {
		return nil
	}
	after, err := fetchPage(ctx, r.Host, Job{Stderr: r.Log})
	if err != nil {
		return fmt.Errorf("reading the app page after the change: %w", err)
	}
	switch {
	case after.Status != s.baseline.Status:
		return fmt.Errorf("the app page answered %d before the change and %d after it", s.baseline.Status, after.Status)
	case after.CSP != s.baseline.CSP:
		return fmt.Errorf("the app page's Content-Security-Policy changed")
	case after.SHA256 != s.baseline.SHA256:
		return fmt.Errorf("the app page's content changed")
	}
	return nil
}

func (s *RustStep) Rollback(ctx context.Context, _ *Run) error {
	if s.host == nil {
		return nil
	}
	if err := s.host.Remove(ctx, []string{stack.RustService}); err != nil {
		return err
	}
	return s.host.Apply(ctx, s.prev)
}

func RustPorts() []int { return slices.Clone(nativeChildren[stack.RustService].ports) }

func waitHealthy(ctx context.Context, h Host, name string, timeout time.Duration) error {
	deadline := time.Now().Add(timeout)
	for {
		svcs, err := h.Services(ctx)
		if err != nil {
			return err
		}
		bad := unhealthy(svcs, name)
		if len(bad) == 0 {
			return nil
		}
		if time.Now().After(deadline) {
			return fmt.Errorf("not healthy after %s: %s", timeout, strings.Join(bad, ", "))
		}
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(pollEvery):
		}
	}
}

func unhealthy(svcs []Service, name string) []string {
	var bad []string
	found := false
	for _, svc := range svcs {
		if svc.Name != name && !strings.HasPrefix(svc.Name, name+"/") {
			continue
		}
		found = found || svc.Name == name
		if !svc.Running || svc.Health != Healthy {
			bad = append(bad, svc.Name)
		}
	}
	if !found {
		bad = append(bad, name)
	}
	return bad
}

func previous(current, changes map[string]string) map[string]string {
	out := make(map[string]string, len(changes))
	for k := range changes {
		out[k] = current[k]
	}
	return out
}
