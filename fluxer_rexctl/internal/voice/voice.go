// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"net/url"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	ServersDir      = "voice"
	EdgeDir         = "edge"
	SitesDir        = "sites.d"
	ComposeFile     = "voice.compose.yml"
	HTTPComposeFile = "voice-http.compose.yml"
	EdgeComposeFile = "voice-edge.compose.yml"
	EdgeCaddyfile   = "Caddyfile.voice"
	EnvFile         = ".env"
	MachineNetwork  = "fluxer-voice"
	MachineProject  = "fluxer-voice"
	EdgeProject     = "fluxer-voice-edge"
	EdgeID          = "edge"
	KeyPrefix       = "fluxer-"
	PathPrefix      = "/livekit-"
	DropInPrefix    = "voice-"
	ContainerPort   = 7880
	FirstTCPPort    = 7881
	FirstUDPPort    = 7882
	DefaultImage    = "livekit/livekit-server:v1.12.0"
	DefaultMemory   = "2g"
	DefaultGrace    = "15m"
	ConnsPerCPU     = 75
)

const (
	EdgeOwn      = "own"
	EdgeInstance = "instance"
	EdgeNone     = "none"
)

var idPattern = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{0,62}$`)

func ValidID(id string) bool { return idPattern.MatchString(id) }

func JoinedProject(id string) string { return MachineProject + "-" + id }

func ProjectName(instanceProject, id string) string {
	return instanceProject + "-voice-" + id
}

func ServerDir(dir, id string) string { return filepath.Join(dir, ServersDir, id) }

func Alias(id string) string { return "voice-" + id }

func Key(id string) string { return KeyPrefix + id }

func Path(id string) string { return PathPrefix + id }

func InternalURL(id string) string {
	return fmt.Sprintf("http://%s:%d", Alias(id), ContainerPort)
}

func NewSecret() (string, error) {
	b := make([]byte, 32)
	if _, err := rand.Read(b); err != nil {
		return "", err
	}
	return hex.EncodeToString(b), nil
}

func NextID(region string, taken []string) string {
	for n := 1; ; n++ {
		id := fmt.Sprintf("%s-server-%d", region, n)
		if !slices.Contains(taken, id) {
			return id
		}
	}
}

type Env struct {
	ID            string
	Key           string
	Secret        string
	WebhookURL    string
	Network       string
	TCPPort       int
	UDPPort       int
	HTTPPort      int
	NodeIP        string
	BindIP        string
	Image         string
	Memory        string
	StopGrace     string
	StunPrimary   string
	StunSecondary string
	LogLevel      string
}

func (e Env) Values() map[string]string {
	useExternal := "true"
	if e.NodeIP != "" {
		useExternal = "false"
	}
	out := map[string]string{
		"FLUXER_VOICE_ID":              e.ID,
		"FLUXER_VOICE_API_KEY":         e.Key,
		"FLUXER_VOICE_API_SECRET":      e.Secret,
		"FLUXER_VOICE_WEBHOOK_URL":     e.WebhookURL,
		"FLUXER_VOICE_NETWORK":         e.Network,
		"FLUXER_VOICE_TCP_PORT":        strconv.Itoa(e.TCPPort),
		"FLUXER_VOICE_UDP_PORT":        strconv.Itoa(e.UDPPort),
		"FLUXER_VOICE_NODE_IP":         e.NodeIP,
		"FLUXER_VOICE_USE_EXTERNAL_IP": useExternal,
		"FLUXER_VOICE_BIND_IP":         e.BindIP,
		"FLUXER_LIVEKIT_IMAGE":         orDefault(e.Image, DefaultImage),
		"FLUXER_VOICE_MEMORY_LIMIT":    orDefault(e.Memory, DefaultMemory),
		"FLUXER_VOICE_GOMEMLIMIT":      GoMemLimit(orDefault(e.Memory, DefaultMemory)),
		"FLUXER_VOICE_STOP_GRACE":      orDefault(e.StopGrace, DefaultGrace),
	}
	if e.HTTPPort > 0 {
		out["FLUXER_VOICE_HTTP_PORT"] = strconv.Itoa(e.HTTPPort)
	}
	for k, v := range map[string]string{
		"FLUXER_LIVEKIT_STUN_PRIMARY":   e.StunPrimary,
		"FLUXER_LIVEKIT_STUN_SECONDARY": e.StunSecondary,
		"FLUXER_LIVEKIT_LOG_LEVEL":      e.LogLevel,
	} {
		if v != "" {
			out[k] = v
		}
	}
	return out
}

func (e Env) File() []byte {
	values := e.Values()
	keys := make([]string, 0, len(values))
	for k := range values {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	var b strings.Builder
	for _, k := range keys {
		fmt.Fprintf(&b, "%s=%s\n", k, values[k])
	}
	return []byte(b.String())
}

func orDefault(v, fallback string) string {
	if strings.TrimSpace(v) == "" {
		return fallback
	}
	return v
}

func ParseBytes(v string) (uint64, bool) {
	v = strings.ToLower(strings.TrimSpace(v))
	units := []struct {
		suffix string
		mult   uint64
	}{{"gib", 1 << 30}, {"mib", 1 << 20}, {"kib", 1 << 10}, {"gb", 1 << 30}, {"mb", 1 << 20}, {"kb", 1 << 10}, {"g", 1 << 30}, {"m", 1 << 20}, {"k", 1 << 10}, {"b", 1}}
	for _, u := range units {
		if n, ok := strings.CutSuffix(v, u.suffix); ok {
			f, err := strconv.ParseFloat(strings.TrimSpace(n), 64)
			if err != nil || f <= 0 {
				return 0, false
			}
			return uint64(f * float64(u.mult)), true
		}
	}
	n, err := strconv.ParseUint(v, 10, 64)
	return n, err == nil && n > 0
}

func GoMemLimit(memory string) string {
	n, ok := ParseBytes(memory)
	if !ok {
		return "1700MiB"
	}
	return strconv.FormatUint(n*85/100>>20, 10) + "MiB"
}

func MachineMemory(hostBytes uint64) string {
	half := hostBytes / 2
	half = max(half, 512<<20)
	half = min(half, 8<<30)
	return strconv.FormatUint(half>>20, 10) + "m"
}

func SoftLimit(cpus int) int {
	return max(1, cpus) * ConnsPerCPU
}

func HumanBytes(n uint64) string {
	if n >= 1<<30 {
		return strings.TrimSuffix(strconv.FormatFloat(float64(n)/(1<<30), 'f', 1, 64), ".0") + " GB"
	}
	return strconv.FormatUint(n>>20, 10) + " MB"
}

func PickPorts(taken map[host.Port]bool, free func(number int, proto string) bool) (int, int) {
	usable := func(n int, proto string) bool {
		return n >= 1 && n <= 65535 && !taken[host.Port{Number: n, Proto: proto}] && (free == nil || free(n, proto))
	}
	for tcp, udp := FirstTCPPort, FirstUDPPort; udp <= 65535; tcp, udp = tcp+2, udp+2 {
		if usable(tcp, "tcp") && usable(udp, "udp") {
			return tcp, udp
		}
	}
	return 0, 0
}

func DropIn(id string) []byte {
	return fmt.Appendf(nil, "handle_path %s/* {\n\timport upstream %s:%d\n}\n", Path(id), Alias(id), ContainerPort)
}

func DropInName(id string) string { return DropInPrefix + id + ".caddy" }

func SiteFileName(address string) string {
	return strings.NewReplacer(":", "_", "/", "_").Replace(address) + ".caddy"
}

func SiteFile(address string, ids []string, ipCertificate bool) []byte {
	var b strings.Builder
	fmt.Fprintf(&b, "%s {\n", address)
	if ipCertificate {
		b.WriteString("\ttls {\n\t\tissuer acme {\n\t\t\tprofile shortlived\n\t\t}\n\t}\n\n")
	}
	b.WriteString("\thandle /_health {\n\t\trespond \"OK\" 200\n\t}\n")
	sorted := slices.Clone(ids)
	slices.Sort(sorted)
	for _, id := range sorted {
		fmt.Fprintf(&b, "\n\thandle_path %s/* {\n\t\timport livekit %s:%d\n\t}\n", Path(id), Alias(id), ContainerPort)
	}
	b.WriteString("\n\thandle {\n\t\trespond 404\n\t}\n}\n")
	return []byte(b.String())
}

func WSOrigin(origin string) string {
	u, err := url.Parse(origin)
	if err != nil || u.Host == "" {
		return ""
	}
	if u.Scheme == "http" {
		return "ws://" + u.Host
	}
	return "wss://" + u.Host
}

func Endpoint(address, id string) string {
	return "wss://" + address + Path(id)
}

func ShippedFile(set *stack.Set, name string) ([]byte, error) {
	return stack.ReadFile(set, ServersDir+"/"+name)
}
