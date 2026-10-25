// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"strconv"
	"strings"
)

var Services = []string{
	"edge", "postgres", "valkey", "nats", "meilisearch", "seaweedfs", "seaweedfs-init", "livekit",
	"api", "worker", "gateway", "media-proxy", "push", "static-proxy", "app-proxy", "admin",
	"snowflakes", "users", "gifs", "messages", "unfurl",
}

var OneShot = map[string]bool{"seaweedfs-init": true}

var Volumes = []string{
	"edge-data", "edge-config", "postgres-data", "valkey-data", "nats-data", "meilisearch-data", "seaweedfs-data",
}

var BackupVolumes = []string{"seaweedfs-data", "valkey-data", "nats-data", "meilisearch-data"}

type DataMount struct {
	Service string
	Target  string
}

var DataMounts = map[string]DataMount{
	"postgres-data":    {Service: "postgres", Target: "/var/lib/postgresql/data"},
	"valkey-data":      {Service: "valkey", Target: "/data"},
	"nats-data":        {Service: "nats", Target: "/data"},
	"meilisearch-data": {Service: "meilisearch", Target: "/meili_data"},
	"seaweedfs-data":   {Service: "seaweedfs", Target: "/data"},
}

var PublishedPorts = []Port{
	{Number: 80, Proto: "tcp"},
	{Number: 443, Proto: "tcp"},
	{Number: 443, Proto: "udp"},
	{Number: 7881, Proto: "tcp"},
	{Number: 7882, Proto: "udp"},
}

type Port struct {
	Number int
	Proto  string
}

const (
	VoiceTCPPortKey = "FLUXER_LIVEKIT_TCP_PORT"
	VoiceUDPPortKey = "FLUXER_LIVEKIT_UDP_PORT"
)

func Ports(env map[string]string) []Port {
	out := make([]Port, 0, len(PublishedPorts))
	for _, p := range PublishedPorts {
		key := ""
		switch {
		case p.Number == 7881 && p.Proto == "tcp":
			key = VoiceTCPPortKey
		case p.Number == 7882 && p.Proto == "udp":
			key = VoiceUDPPortKey
		}
		if n, err := strconv.Atoi(strings.TrimSpace(env[key])); key != "" && err == nil && n > 0 && n <= 65535 {
			p.Number = n
		}
		out = append(out, p)
	}
	return out
}
