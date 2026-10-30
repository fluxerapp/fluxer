// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func TestJoinInstallsDockerOnBareLinux(t *testing.T) {
	unreachable := engineFailure(&engine.Error{Op: "connect", Class: engine.ClassUnreachable})
	denied := engineFailure(&engine.Error{Op: "connect", Class: engine.ClassDenied})
	cases := []struct {
		name   string
		err    error
		goos   string
		dryRun bool
		want   bool
	}{
		{"no engine on linux", unreachable, "linux", false, true},
		{"dry run", unreachable, "linux", true, false},
		{"macOS", unreachable, "darwin", false, false},
		{"permission denied", denied, "linux", false, false},
		{"cancelled", context.Canceled, "linux", false, false},
	}
	for _, c := range cases {
		if got := joinInstallsDocker(c.err, c.goos, c.dryRun); got != c.want {
			t.Errorf("%s: got %v, want %v", c.name, got, c.want)
		}
	}
}

func TestVoiceServersFollowThePinnedLiveKitImage(t *testing.T) {
	inst := &instance.Instance{Dir: t.TempDir(), State: &instance.State{
		Settings: map[string]string{},
		Images:   instance.Images{Release: "2026.1030.2", Refs: map[string]string{"livekit": "livekit/livekit-server:v1.13.11"}},
	}}
	if got := instanceLiveKitImage(inst); got != "livekit/livekit-server:v1.13.11" {
		t.Errorf("pinned: got %s", got)
	}
	inst.State.Settings["images.livekit"] = "registry.example.com/livekit:v1"
	if got := instanceLiveKitImage(inst); got != "registry.example.com/livekit:v1" {
		t.Errorf("operator setting: got %s", got)
	}
}

func TestUpgradeLeavesOffAndDrainingServersAsTheyWere(t *testing.T) {
	cases := []struct {
		active, draining, want bool
	}{
		{true, false, true},
		{false, false, false},
		{true, true, false},
		{false, true, false},
	}
	for _, c := range cases {
		if got := upgradeDrains(c.active, c.draining); got != c.want {
			t.Errorf("active %v, draining %v: got %v", c.active, c.draining, got)
		}
	}
}

func TestJoinTakesIPForAShared100Dot64Route(t *testing.T) {
	local := []string{"100.64.12.7"}
	_, err := assessJoin("203.0.113.9", "", local, "100.64.12.7")
	var ee *ui.ExitError
	if !errors.As(err, &ee) || !strings.Contains(ee.Fix, "--ip") {
		t.Fatalf("without --ip: %v", err)
	}
	reach, err := assessJoin("203.0.113.9", "203.0.113.9", local, "100.64.12.7")
	if err != nil {
		t.Fatalf("with --ip: %v", err)
	}
	if reach.CGNAT || !reach.BehindNAT || reach.PublicIPv4 != "203.0.113.9" {
		t.Fatalf("reach = %+v", reach)
	}
	if bindAddress("203.0.113.9", local) != "" {
		t.Fatal("bound to an address this machine doesn't have")
	}
}

func TestNewRegionsNeedCoordinates(t *testing.T) {
	app, _ := testVoiceApp()
	for _, args := range [][]string{
		{"--new-region", "fra", "--region-name", "Frankfurt"},
		{"--new-region", "fra", "--region-name", "Frankfurt", "--lat", "50.1"},
		{"--new-region", "fra", "--region-name", "Frankfurt", "--lat", "95", "--long", "8.7"},
	} {
		c := newVoiceAddCmd(app)
		c.SetArgs(args)
		c.SilenceUsage, c.SilenceErrors = true, true
		err := c.Execute()
		var ee *ui.ExitError
		if !errors.As(err, &ee) || ee.Code != ui.ExitUsage || !strings.Contains(ee.Msg, "--lat") {
			t.Errorf("%v: %v", args, err)
		}
	}
	c := newVoiceTokenCmd(app)
	c.SetArgs([]string{"--region", "fra", "--lat", "50.1", "--long", "8.7"})
	c.SilenceUsage, c.SilenceErrors = true, true
	if err := c.Execute(); err == nil || !strings.Contains(err.Error(), "--new-region") {
		t.Errorf("token coordinates without a new region: %v", err)
	}
}

func TestVoiceListShowsRegionsWithoutServers(t *testing.T) {
	inst := &instance.Instance{State: &instance.State{Settings: map[string]string{}}}
	regions := []adminapi.VoiceRegion{
		{VoiceRegionAdminResponse: adminapi.VoiceRegionAdminResponse{Id: "default"}, Servers: []adminapi.VoiceServerAdminResponse{{ServerId: "default-server-1", IsActive: true}}},
		{VoiceRegionAdminResponse: adminapi.VoiceRegionAdminResponse{Id: "fra"}},
	}
	rows, servers := voiceListRows(regions, inst)
	if servers != 1 || len(rows) != 2 || rows[1][0] != "-" || rows[1][1] != "fra" || rows[1][2] != "no servers" {
		t.Fatalf("rows = %v, servers = %d", rows, servers)
	}
}

func TestJoinRefusesOlderPodmanWithoutAFluxerInstance(t *testing.T) {
	old := &engine.Engine{Kind: engine.Podman, Version: "5.4.2"}
	if err := podmanJoinRefusal(old, &joinPlan{}); err == nil {
		t.Fatal("Podman 5.4.2 on a voice machine was allowed")
	}
	if err := podmanJoinRefusal(old, &joinPlan{edgeInst: &instance.Instance{}}); err != nil {
		t.Fatalf("a server next to Fluxer was refused: %v", err)
	}
	if err := podmanJoinRefusal(&engine.Engine{Kind: engine.Podman, Version: podmanRestartsContainers}, &joinPlan{}); err != nil {
		t.Fatalf("Podman %s was refused: %v", podmanRestartsContainers, err)
	}
	if err := podmanJoinRefusal(&engine.Engine{Kind: engine.Docker, Version: "27.0.0"}, &joinPlan{}); err != nil {
		t.Fatalf("Docker was refused: %v", err)
	}
}

func TestSysctlCommandsMatchTheComposePage(t *testing.T) {
	app, out := testVoiceApp()
	showVoiceSysctlCommands(app)
	for _, want := range []string{
		`printf 'net.core.rmem_max = 16777216\nnet.core.wmem_max = 16777216\n' | sudo tee /etc/sysctl.d/61-fluxer-voice.conf`,
		"sudo sysctl -p /etc/sysctl.d/61-fluxer-voice.conf",
	} {
		if !strings.Contains(out.String(), want) {
			t.Fatalf("missing %q in %s", want, out.String())
		}
	}
}
