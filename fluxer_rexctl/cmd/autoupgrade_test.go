// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func notifyRun(t *testing.T, notify bool) (*App, chan upgradeNotice) {
	t.Helper()
	got := make(chan upgradeNotice, 1)
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		var n upgradeNotice
		if r.Method != http.MethodPost || r.Header.Get("Content-Type") != "application/json" || json.NewDecoder(r.Body).Decode(&n) != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		got <- n
	}))
	t.Cleanup(srv.Close)
	inst, err := instance.Create(t.TempDir(), "fluxer")
	if err != nil {
		t.Fatal(err)
	}
	inst.State.Release = "2026.1027.120000"
	if notify {
		inst.State.NotifyURL = srv.URL + "/hooks/secret"
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	out := &bytes.Buffer{}
	app := &App{
		Flags: GlobalFlags{Dir: inst.Dir},
		UI:    ui.New(ui.Options{Stdin: strings.NewReader(""), Stdout: out, Stderr: out}),
	}
	return app, got
}

func TestNotifyUpgrade(t *testing.T) {
	ctx := context.Background()

	app, got := notifyRun(t, true)
	app.outcome = "Upgraded to Fluxer 2026.1027.120000."
	notifyUpgrade(ctx, app, nil)
	n := <-got
	if n.Status != "upgraded" || n.Release != "2026.1027.120000" || n.Text != n.Content || !strings.HasSuffix(n.Text, "Upgraded to Fluxer 2026.1027.120000.") {
		t.Errorf("upgraded notice = %+v", n)
	}

	app, got = notifyRun(t, true)
	notifyUpgrade(ctx, app, ui.Refused("Another rexctl command is changing this instance.", "Wait for it to finish, then try again."))
	n = <-got
	if n.Status != "failed" || !strings.HasSuffix(n.Text, "Another rexctl command is changing this instance. Wait for it to finish, then try again.") {
		t.Errorf("failed notice = %+v", n)
	}

	app, got = notifyRun(t, true)
	app.outcome = "Fluxer 2026.1027.120000 didn't start. Went back to Fluxer 2026.1020.100000."
	app.SetExit(ui.ExitFailed)
	notifyUpgrade(ctx, app, nil)
	if n = <-got; n.Status != "failed" {
		t.Errorf("went-back notice = %+v", n)
	}

	app, got = notifyRun(t, true)
	notifyUpgrade(ctx, app, nil)
	notifyUpgrade(ctx, app, context.Canceled)
	app, _ = notifyRun(t, false)
	app.outcome = "Upgraded to Fluxer 2026.1027.120000."
	notifyUpgrade(ctx, app, nil)
	select {
	case n := <-got:
		t.Errorf("sent %+v for a run with nothing to tell", n)
	default:
	}
}

func TestMaskURL(t *testing.T) {
	cases := map[string]string{
		"https://discord.com/api/webhooks/1/secret": "https://discord.com/…",
		"https://ntfy.sh/topic?tpl=1":               "https://ntfy.sh/…",
		"https://hooks.example.com":                 "https://hooks.example.com",
	}
	for in, want := range cases {
		if got := maskURL(in); got != want {
			t.Errorf("maskURL(%s) = %s, want %s", in, got, want)
		}
	}
}

func TestAutoUpgradeWhen(t *testing.T) {
	if got := autoUpgradeWhen(host.Systemd, "23:45"); got != "between 23:45 and 00:15" {
		t.Errorf("systemd: %s", got)
	}
	if got := autoUpgradeWhen(host.Cron, "04:00"); got != "at 04:00" {
		t.Errorf("cron: %s", got)
	}
}
