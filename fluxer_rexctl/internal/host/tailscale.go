// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"encoding/json"
	"net/netip"
	"os/exec"
	"strings"
	"time"
)

type Tailnet struct {
	Name string
	IPv4 string
}

func DetectTailnet(ctx context.Context) Tailnet {
	ctx, cancel := context.WithTimeout(ctx, 3*time.Second)
	defer cancel()
	out, err := exec.CommandContext(ctx, "tailscale", "status", "--json").Output()
	if err != nil {
		return Tailnet{}
	}
	return parseTailnet(out)
}

func parseTailnet(data []byte) Tailnet {
	var st struct {
		BackendState string
		CertDomains  []string
		Self         struct{ TailscaleIPs []string }
	}
	if json.Unmarshal(data, &st) != nil || st.BackendState != "Running" || len(st.CertDomains) == 0 {
		return Tailnet{}
	}
	t := Tailnet{Name: strings.TrimSuffix(st.CertDomains[0], ".")}
	for _, s := range st.Self.TailscaleIPs {
		if ip, err := netip.ParseAddr(s); err == nil && ip.Is4() {
			t.IPv4 = ip.String()
			break
		}
	}
	return t
}
