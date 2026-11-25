// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"cmp"
	"context"
	"fmt"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
)

func init() {
	Register(Check{ID: "push-relay", Phases: Installed, Run: runPushRelay})
}

func runPushRelay(ctx context.Context, e *Env) Result {
	if e.Admin == nil || e.state() == nil || !e.state().SetupDone || !e.running(ctx, "api") {
		return skip("")
	}
	cfg, err := e.Admin.Config(ctx)
	if err != nil {
		return skip("")
	}
	return pushRelayResult(cfg.PushRelay, e.derivedOrigin()+cmp.Or(e.value("FLUXER_ADMIN_BASE_PATH"), "/admin"))
}

func pushRelayResult(relay adminapi.PushRelayConfigResponse, admin string) Result {
	if relay.RelayConsentAccepted {
		return pass("The official mobile apps get notifications through Fluxer's push relay.")
	}
	return warn("The official mobile apps get no notifications from this instance, because nobody has accepted the push relay privacy notice.",
		fmt.Sprintf("If you want them, read https://fluxer.com/push-relay and accept it under Push Relay at %s/instance-config. Otherwise you can leave this as it is.", admin))
}
