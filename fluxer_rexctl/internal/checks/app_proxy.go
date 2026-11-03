// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"strings"
)

func init() {
	Register(Check{ID: "app-proxy-discovery", Phases: Installed, Run: runAppProxy, Fix: fixAppProxy, SafeFix: true})
}

func runAppProxy(ctx context.Context, e *Env) Result {
	if e.Instance == nil || !e.running(ctx, "edge") || !e.running(ctx, "app-proxy") {
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
	return e.Project.Restart(ctx, "app-proxy")
}
