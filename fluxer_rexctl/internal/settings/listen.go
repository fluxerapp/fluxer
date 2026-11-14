// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"cmp"
	"fmt"
	"maps"
	"net"
	"slices"
	"strings"
)

var edgePortVars = map[string]string{"FLUXER_HTTP_PORT": "80", "FLUXER_HTTPS_PORT": "443"}

func EdgeDialAddr(listen, port string) string {
	if h, p, err := net.SplitHostPort(port); err == nil {
		listen, port = h, p
	}
	host := "127.0.0.1"
	if ip := net.ParseIP(strings.TrimSpace(listen)); ip != nil && !ip.IsUnspecified() {
		host = ip.String()
	} else if ip != nil && ip.To4() == nil {
		host = "::1"
	}
	return net.JoinHostPort(host, port)
}

type ListenConflict struct {
	Addresses map[string]string
	Ports     map[string]string
}

func (c *ListenConflict) Error() string {
	var parts []string
	for _, k := range slices.Sorted(maps.Keys(c.Addresses)) {
		parts = append(parts, k+" uses "+c.Addresses[k])
	}
	last := len(parts) - 1
	return strings.Join(parts[:last], ", ") + " and " + parts[last] + ", but Fluxer listens on one address for HTTP and HTTPS."
}

func (c *ListenConflict) Fix() string {
	keep := cmp.Or(c.Addresses[EdgeListen], c.Addresses[EdgeHTTPSPort], c.Addresses[EdgeHTTPPort])
	var pairs []string
	for _, key := range []string{EdgeHTTPPort, EdgeHTTPSPort} {
		if a, ok := c.Addresses[key]; ok && a != keep {
			pairs = append(pairs, key+"="+c.Ports[key])
		}
	}
	return fmt.Sprintf("Run rexctl config set %s to listen on %s for both.", strings.Join(pairs, " "), keep)
}

func SplitListenAddress(values map[string]string, skip []string) ([]string, error) {
	addresses := map[string]string{}
	ports := map[string]string{EdgeHTTPPort: "80", EdgeHTTPSPort: "443"}
	var split []string
	for _, key := range []string{EdgeHTTPPort, EdgeHTTPSPort} {
		v, ok := values[key]
		if !ok || slices.Contains(skip, key) {
			continue
		}
		ports[key] = v
		if validPort(v) == nil {
			continue
		}
		host, port, err := net.SplitHostPort(strings.TrimSpace(v))
		ip := net.ParseIP(host)
		if err != nil || ip == nil || validPort(port) != nil {
			continue
		}
		addresses[key], ports[key] = ip.String(), port
		split = append(split, key)
	}
	if len(split) == 0 {
		return nil, nil
	}
	if listen, ok := values[EdgeListen]; ok && listen != "" && !slices.Contains(skip, EdgeListen) {
		if ip := net.ParseIP(listen); ip != nil {
			addresses[EdgeListen] = ip.String()
		} else {
			addresses[EdgeListen] = listen
		}
	}
	first := addresses[split[0]]
	for _, a := range addresses {
		if a != first {
			return nil, &ListenConflict{Addresses: addresses, Ports: ports}
		}
	}
	for _, key := range split {
		values[key] = ports[key]
	}
	values[EdgeListen] = first
	return split, nil
}
