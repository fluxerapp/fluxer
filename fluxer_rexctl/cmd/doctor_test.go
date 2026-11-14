// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const testDoctorPhase checks.Phase = 1 << 10

func init() {
	checks.Register(checks.Check{
		ID:      "test-failing-fix",
		Phases:  testDoctorPhase,
		SafeFix: true,
		Run: func(context.Context, *checks.Env) checks.Result {
			return checks.Result{Status: checks.Fail, Message: "The test folder isn't writable.", Fix: "Run rexctl doctor --fix to make it writable.", Fixable: true, Details: []string{""}}
		},
		Fix: func(context.Context, *checks.Env) error {
			return errors.New("chmod: /spool: Operation not permitted")
		},
	})
}

func TestDoctorShowsWhyAFixFailed(t *testing.T) {
	var during, report bytes.Buffer
	app := &App{UI: ui.New(ui.Options{Verbose: true, Stdout: &report, Stderr: &during})}
	env := &checks.Env{Dir: t.TempDir()}
	env.SetEngine(nil, errors.New("no engine in tests"))
	results := diagnose(context.Background(), app, env, testDoctorPhase, true, false)
	app.UI = ui.New(ui.Options{Verbose: true, Stdout: &report, Stderr: &report})
	if err := reportDoctor(app, results, true, false); err != nil {
		t.Fatal(err)
	}
	lines := strings.Split(strings.TrimRight(report.String(), "\n"), "\n")
	want := []string{
		"x The test folder isn't writable.",
		"  rexctl couldn't fix this. Run rexctl doctor --fix --verbose to see why.",
		"  chmod: /spool: Operation not permitted",
	}
	if len(lines) < len(want) || strings.Join(lines[:len(want)], "\n") != strings.Join(want, "\n") {
		t.Fatalf("report:\n%s\nwant it to start with:\n%s", report.String(), strings.Join(want, "\n"))
	}
	for _, l := range lines {
		if strings.TrimSpace(l) == "" {
			t.Fatalf("report has a blank line:\n%s", report.String())
		}
	}
}
