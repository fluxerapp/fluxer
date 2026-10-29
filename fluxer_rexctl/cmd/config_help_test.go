// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestRawSettingHelp(t *testing.T) {
	set := stack.Embedded()
	raw := 0
	for _, name := range set.Vars {
		k, err := settings.Lookup(set, nil, name, nil)
		if err != nil || k.Kind != settings.Raw {
			continue
		}
		raw++
		help := keyHelp(k)
		if help == "" {
			t.Errorf("rexctl config get %s shows no help", name)
		}
		if strings.Contains(help, "docker compose ") || strings.Contains(help, "Keep this line") {
			t.Errorf("rexctl config get %s shows help for Docker Compose users, got %q", name, help)
		}
	}
	if raw == 0 {
		t.Fatal("no compose setting resolves as a raw setting")
	}
}
