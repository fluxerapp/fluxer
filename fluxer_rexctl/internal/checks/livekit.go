// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bytes"
	"context"
	"fmt"
	"net"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func init() {
	Register(Check{ID: "livekit-ip", Phases: Installed, Run: runLiveKitIP, Fix: fixLiveKitIP})
}

func runLiveKitIP(ctx context.Context, e *Env) Result {
	if e.Instance == nil || !e.running(ctx, "livekit") {
		return skip("")
	}
	public, perr := e.publicAddress(ctx)
	setTo := "Run rexctl doctor --fix to set it to " + public + "."
	if perr != nil {
		setTo = "Make sure this machine can send UDP traffic to the internet, then run rexctl doctor."
	}
	nodeIP := e.value("FLUXER_LIVEKIT_NODE_IP")
	external := !strings.EqualFold(e.value("FLUXER_LIVEKIT_USE_EXTERNAL_IP"), "false")
	if !external {
		if nodeIP == "" {
			r := fail("Voice calls use an address that only works inside this machine.", setTo)
			r.Fixable = perr == nil
			return r
		}
		ip := net.ParseIP(nodeIP)
		if perr == nil && ip != nil && !ip.IsPrivate() && !ip.Equal(net.ParseIP(public)) {
			return fixable(warn(fmt.Sprintf("Voice calls use %s and this machine's public IP address is %s.", nodeIP, public),
				"Run rexctl doctor --fix to use "+public+"."))
		}
		return pass(fmt.Sprintf("Voice calls use %s.", nodeIP))
	}
	var logs bytes.Buffer
	if err := e.Project.Logs(ctx, engine.LogsOptions{Tail: 300, Stdout: &logs}, "livekit"); err != nil {
		return skip("")
	}
	if strings.Contains(strings.ToLower(logs.String()), "could not resolve external ip") {
		r := fail("Voice calls can't find this machine's public IP address.", setTo)
		r.Fixable = perr == nil
		return r
	}
	return pass("Voice calls can find this machine's public IP address.")
}

func fixLiveKitIP(ctx context.Context, e *Env) error {
	if e.state() == nil {
		return errNoProject
	}
	public, err := e.publicAddress(ctx)
	if err != nil {
		return err
	}
	e.stage(settings.VoicePublicIP, &public)
	return nil
}
