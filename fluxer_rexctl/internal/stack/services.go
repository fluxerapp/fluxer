// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"strconv"
	"strings"
)

var Services = []string{
	"edge", "postgres", "valkey", "nats", "meilisearch", "seaweedfs", "seaweedfs-init", "livekit",
	"api", "worker", "gateway", "media-proxy", "push", "static-proxy", "app-proxy", "admin",
	"snowflakes", "users", "gifs", "messages", "unfurl", RustService, ObjectsInitService,
}

const (
	ObjectsInitService = "objects-init"
	ObjectsVolume      = "objects-data"
	SeaweedFSVolume    = "seaweedfs-data"
	ValkeyVolume       = "valkey-data"
	ObjectsRoot        = "/var/lib/fluxer/objects"
	ObjectsLayout      = `{"v":1}`
	KVModeVar          = "FLUXER_KV_MODE"
	WorkerReplicasVar  = "FLUXER_WORKER_REPLICAS"
)

var OneShot = map[string]bool{"seaweedfs-init": true, ObjectsInitService: true}

var Volumes = []string{
	"edge-data", "edge-config", "postgres-data", ValkeyVolume, "nats-data", "meilisearch-data", SeaweedFSVolume, ObjectsVolume,
}

var BackupVolumes = []string{SeaweedFSVolume, ObjectsVolume, ValkeyVolume, "nats-data", "meilisearch-data"}

type DataMount struct {
	Service string
	Target  string
}

var DataMounts = map[string]DataMount{
	"postgres-data":    {Service: "postgres", Target: "/var/lib/postgresql/data"},
	ValkeyVolume:       {Service: "valkey", Target: "/data"},
	"nats-data":        {Service: "nats", Target: "/data"},
	"meilisearch-data": {Service: "meilisearch", Target: "/meili_data"},
	SeaweedFSVolume:    {Service: "seaweedfs", Target: "/data"},
	ObjectsVolume:      {Service: "media-proxy", Target: ObjectsRoot},
}

func FilesystemStorage(settings map[string]string) bool {
	return strings.TrimSpace(settings["storage.backend"]) == "filesystem"
}

func KVPostgres(settings map[string]string) bool {
	return strings.TrimSpace(settings["kv.mode"]) == "postgres"
}

func UploadsVolume(settings map[string]string) string {
	if FilesystemStorage(settings) {
		return ObjectsVolume
	}
	return SeaweedFSVolume
}

func UploadsServices(settings map[string]string) []string {
	if FilesystemStorage(settings) {
		return []string{ObjectsInitService}
	}
	return []string{"seaweedfs", "seaweedfs-init"}
}

func SkippedVolumes(settings map[string]string) []string {
	var out []string
	if FilesystemStorage(settings) {
		out = append(out, SeaweedFSVolume)
	} else {
		out = append(out, ObjectsVolume)
	}
	if KVPostgres(settings) {
		out = append(out, ValkeyVolume)
	}
	return out
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
	VoiceBundledKey = "FLUXER_LIVEKIT_BUNDLED"
)

func Ports(env map[string]string) []Port {
	out := make([]Port, 0, len(PublishedPorts))
	bundled := VoiceBundled(env[VoiceBundledKey])
	for _, p := range PublishedPorts {
		key := ""
		switch {
		case p.Number == 7881 && p.Proto == "tcp":
			key = VoiceTCPPortKey
		case p.Number == 7882 && p.Proto == "udp":
			key = VoiceUDPPortKey
		}
		if key != "" && !bundled {
			continue
		}
		if n, err := strconv.Atoi(strings.TrimSpace(env[key])); key != "" && err == nil && n > 0 && n <= 65535 {
			p.Number = n
		}
		out = append(out, p)
	}
	return out
}
