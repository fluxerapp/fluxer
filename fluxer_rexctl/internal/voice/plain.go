// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"fmt"
	"strings"
)

type Plain struct {
	ID      string
	Key     string
	Secret  string
	Webhook string
	TCPPort int
	UDPPort int
	NodeIP  string
	Turn    string
}

func (p Plain) YAML() string {
	var b strings.Builder
	fmt.Fprintf(&b, "# LiveKit config for the Fluxer voice server %s.\n", p.ID)
	b.WriteString("port: 7880\nlog_level: info\n")
	fmt.Fprintf(&b, "rtc:\n  tcp_port: %d\n  udp_port: %d\n", p.TCPPort, p.UDPPort)
	if p.NodeIP != "" {
		fmt.Fprintf(&b, "  use_external_ip: false\n  node_ip: %q\n", p.NodeIP)
	} else {
		b.WriteString("  use_external_ip: true\n")
	}
	fmt.Fprintf(&b, "  stun_servers:\n    - %s\n    - %s\n", defaultStunPrimary, defaultStunBackup)
	if p.Turn != "" {
		fmt.Fprintf(&b, "  turn_servers: %s\n", p.Turn)
	}
	b.WriteString("turn:\n  enabled: false\n")
	fmt.Fprintf(&b, "keys:\n  %s: %s\n", p.Key, p.Secret)
	fmt.Fprintf(&b, "webhook:\n  api_key: %s\n  urls:\n    - %s\n", p.Key, p.Webhook)
	return b.String()
}
