// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"net/url"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

func waitForPublicEndpoint(ctx context.Context, endpoint string, limit time.Duration) error {
	u, err := url.Parse(endpoint)
	if err != nil {
		return err
	}
	return waitForEndpoint(ctx, voice.HTTPBase(endpoint), u.Host+portSuffix(u), false, limit)
}

func portSuffix(u *url.URL) string {
	if u.Port() != "" {
		return ""
	}
	return ":443"
}

type originState int

const (
	originUnknown originState = iota
	originUnlisted
	originPending
	originAllowed
)

func voiceOriginState(ctx context.Context, client *voice.Client, domain, origin string) originState {
	disc, err := client.Discover(ctx, domain)
	switch {
	case err != nil:
		return originUnknown
	case !disc.ListsOrigins:
		return originUnlisted
	case !slices.ContainsFunc(disc.VoiceOrigins, func(source string) bool { return voice.SourceCovers(source, origin) }):
		return originPending
	}
	if ok, err := client.AllowsOrigin(ctx, domain, origin); err == nil && ok {
		return originAllowed
	}
	return originPending
}

func endpointOrigin(endpoint string) string {
	u, err := url.Parse(endpoint)
	if err != nil || u.Host == "" {
		return ""
	}
	return u.Scheme + "://" + u.Host
}

func waitForVoiceOrigin(ctx context.Context, app *App, client *voice.Client, domain, endpoint string) {
	origin := endpointOrigin(endpoint)
	if origin == "" {
		return
	}
	state := originUnknown
	deadline := time.Now().Add(3 * time.Minute)
	for time.Now().Before(deadline) {
		if state = voiceOriginState(ctx, client, domain, origin); state == originAllowed {
			app.UI.OK("Browsers on %s can connect to %s.", domain, origin)
			return
		}
		select {
		case <-ctx.Done():
			return
		case <-time.After(5 * time.Second):
		}
	}
	app.UI.Warn(fmt.Sprintf("Browsers on %s can't connect to %s yet.", domain, origin), originFix(state, domain, app.Version))
}

func originFix(state originState, domain, version string) string {
	if state == originUnlisted {
		target := "the latest release"
		if release.Valid(version) {
			target = version
		}
		return fmt.Sprintf("Upgrade Fluxer on %s to %s, then reload open pages.", domain, target)
	}
	return "Check again in a minute with rexctl voice status. Pages opened before then need a reload."
}
