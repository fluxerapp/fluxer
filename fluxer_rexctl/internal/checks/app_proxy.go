// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func init() {
	Register(Check{ID: "app-proxy-discovery", Phases: Installed, Run: runAppProxy, Fix: fixAppProxy, SafeFix: true})
}

func (e *Env) appProxy() string {
	return stack.RunsIn("app-proxy", stack.RustSingle(map[string]string{settings.RustProcess: e.chosen(settings.RustProcess)}))
}

func runAppProxy(ctx context.Context, e *Env) Result {
	if e.Instance == nil || !e.edgeUp(ctx) || !e.running(ctx, e.appProxy()) {
		return skip("")
	}
	out, err := e.webApp(ctx)
	if err == nil {
		return pass("The web app loads.")
	}
	r := fixable(fail("The web app isn't loading.", "Run rexctl doctor --fix to restart it."))
	if strings.Contains(out, "503") {
		r.Message = "The web app shows an error page."
	}
	r.Details = []string{out}
	return r
}

func (e *Env) webApp(ctx context.Context) (string, error) {
	if !e.Native {
		return e.exec(ctx, "edge", "", "wget", "-q", "-O", "/dev/null", "http://app-proxy:8080/")
	}
	code, _, err := e.viaEdge(ctx, "/")
	if err == nil && (code < 200 || code > 399) {
		return fmt.Sprintf("HTTP %d", code), fmt.Errorf("the web app returned HTTP %d", code)
	}
	if err != nil {
		return err.Error(), err
	}
	return "", nil
}

func fixAppProxy(ctx context.Context, e *Env) error {
	svc := e.appProxy()
	if e.Native && e.Restart != nil && e.Parts != nil {
		parts := e.Parts([]string{svc})
		return e.run(ctx, host.Step{
			Do:  "Restarts " + ui.JoinAnd(parts) + ".",
			Run: func(ctx context.Context) error { return e.Restart(ctx, parts) },
		})
	}
	if e.Project == nil {
		return errNoProject
	}
	do := "Restarts app-proxy."
	if svc != "app-proxy" {
		do = "Restarts " + svc + ", which runs app-proxy."
	}
	return e.run(ctx, host.Step{
		Do:  do,
		Run: func(ctx context.Context) error { return e.Project.Restart(ctx, svc) },
	})
}
