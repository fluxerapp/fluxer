// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func init() {
	Register(Check{ID: "app-proxy-discovery", Phases: Installed, Run: runAppProxy, Fix: fixAppProxy, SafeFix: true})
}

func (e *Env) appProxy() string {
	return stack.RunsIn("app-proxy", stack.RustSingle(map[string]string{settings.RustProcess: e.chosen(settings.RustProcess)}))
}

func runAppProxy(ctx context.Context, e *Env) Result {
	if e.Instance == nil || !e.running(ctx, "edge") || !e.running(ctx, e.appProxy()) {
		return skip("")
	}
	out, err := e.exec(ctx, "edge", "", "wget", "-q", "-O", "/dev/null", "http://app-proxy:8080/")
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

func fixAppProxy(ctx context.Context, e *Env) error {
	if e.Project == nil {
		return errNoProject
	}
	svc := e.appProxy()
	do := "Restarts app-proxy."
	if svc != "app-proxy" {
		do = "Restarts " + svc + ", which runs app-proxy."
	}
	return e.run(ctx, host.Step{
		Do:  do,
		Run: func(ctx context.Context) error { return e.Project.Restart(ctx, svc) },
	})
}
