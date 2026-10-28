// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"errors"
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type Kind int

const (
	Deployment Kind = iota
	Instance
	Raw
)

type Key struct {
	Name     string
	Kind     Kind
	Type     string
	Vars     []string
	Secret   bool
	Stateful bool
	Confirm  string
	Validate func(string) error
	Options  []string
	Help     string
}

const (
	Domain          = "domain"
	EdgeMode        = "edge.mode"
	EdgeBind        = "edge.bind"
	EdgeHTTPPort    = "edge.http_port"
	EdgeHTTPSPort   = "edge.https_port"
	EdgePublicPort  = "edge.public_port"
	EdgeTrusted     = "edge.trusted_proxies"
	VoiceTCPPort    = "voice.tcp_port"
	VoiceUDPPort    = "voice.udp_port"
	VoicePublicIP   = "voice.public_ip"
	StorageEndpoint = "storage.endpoint"
	StoragePublic   = "storage.public_endpoint"
	StorageRegion   = "storage.region"
	StoragePath     = "storage.path_style"
	StorageAccess   = "storage.access_key"
	StorageSecret   = "storage.secret_key"
	HTTPProxy       = "network.http_proxy"
	HTTPSProxy      = "network.https_proxy"
	NoProxy         = "network.no_proxy"
	IPv6            = "network.ipv6"
	SSOPrivate      = "sso.allow_private_addresses"
	ImagesRegistry  = "images.registry"
	MemoryProfile   = "memory.profile"
	EngineHost      = "engine.host"
	ImagesPrefix    = "images."
)

const (
	EdgeBundled = "bundled"
	EdgeProxy   = "proxy"
	EdgeTunnel  = "tunnel"
)

var ImageVars = map[string]string{
	"postgres":    "FLUXER_POSTGRES_IMAGE",
	"valkey":      "FLUXER_VALKEY_IMAGE",
	"nats":        "FLUXER_NATS_IMAGE",
	"meilisearch": "FLUXER_MEILISEARCH_IMAGE",
	"seaweedfs":   "FLUXER_SEAWEEDFS_IMAGE",
	"livekit":     "FLUXER_LIVEKIT_IMAGE",
	"edge":        "FLUXER_CADDY_IMAGE",
}

const storageConfirm = "Files people already uploaded stay where they are. rexctl doesn't move them."

var named = buildNamed()

func buildNamed() []Key {
	keys := []Key{
		{Name: Domain, Type: "hostname", Vars: []string{"FLUXER_DOMAIN"}, Validate: validHostname,
			Help:    "The domain Fluxer runs on",
			Confirm: "Fluxer gets a new certificate for the new domain. Passkeys made for the old domain stop working. The domain_migration settings help people move over."},
		{Name: EdgeMode, Type: "choice", Options: []string{EdgeBundled, EdgeProxy, EdgeTunnel}, Validate: oneOf(EdgeBundled, EdgeProxy, EdgeTunnel),
			Help: "Whether Fluxer serves HTTPS itself, sits behind your reverse proxy or behind a tunnel"},
		{Name: EdgeBind, Type: "address", Vars: []string{"FLUXER_EDGE_BIND"}, Validate: validBind,
			Help: "The address your reverse proxy or tunnel connects to"},
		{Name: EdgeHTTPPort, Type: "port", Vars: []string{"FLUXER_HTTP_PORT"}, Validate: validPort,
			Help: "The port Fluxer listens on for HTTP"},
		{Name: EdgeHTTPSPort, Type: "port", Vars: []string{"FLUXER_HTTPS_PORT"}, Validate: validPort,
			Help: "The port Fluxer listens on for HTTPS"},
		{Name: EdgePublicPort, Type: "port", Vars: []string{"FLUXER_PUBLIC_PORT"}, Validate: validPort,
			Help: "The port people use in Fluxer's address, when it differs from the one Fluxer listens on"},
		{Name: EdgeTrusted, Type: "list", Vars: []string{"FLUXER_EDGE_TRUSTED_PROXIES"}, Validate: validTrustedProxies,
			Help: "Proxies whose client address headers Fluxer trusts"},
		{Name: VoiceTCPPort, Type: "port", Vars: []string{"FLUXER_LIVEKIT_TCP_PORT"}, Validate: validPort,
			Help: "The TCP port for voice and video"},
		{Name: VoiceUDPPort, Type: "port", Vars: []string{"FLUXER_LIVEKIT_UDP_PORT"}, Validate: validPort,
			Help: "The UDP port for voice and video"},
		{Name: VoicePublicIP, Type: "ip", Vars: []string{"FLUXER_LIVEKIT_NODE_IP", "FLUXER_LIVEKIT_USE_EXTERNAL_IP"}, Validate: validIP,
			Help: "The public IP address voice and video use, when Fluxer can't find it by itself"},
		{Name: StorageEndpoint, Type: "url", Vars: []string{"FLUXER_S3_ENDPOINT"}, Validate: validURL, Confirm: storageConfirm,
			Help: "Your own S3-compatible object store"},
		{Name: StoragePublic, Type: "url", Vars: []string{"FLUXER_S3_PUBLIC_ENDPOINT"}, Validate: validURL, Confirm: storageConfirm,
			Help: "The object store address browsers use, when it differs from the one Fluxer uses"},
		{Name: StorageRegion, Type: "text", Vars: []string{"FLUXER_S3_REGION"}, Validate: validText, Confirm: storageConfirm,
			Help: "The object store region"},
		{Name: StoragePath, Type: "bool", Vars: []string{"FLUXER_S3_FORCE_PATH_STYLE"}, Validate: validBool, Confirm: storageConfirm,
			Help: "Whether the object store needs path-style addresses"},
		{Name: StorageAccess, Type: "text", Vars: []string{"FLUXER_S3_ACCESS_KEY"}, Validate: validText, Confirm: storageConfirm,
			Help: "The object store access key"},
		{Name: StorageSecret, Type: "secret", Vars: []string{"FLUXER_S3_SECRET_KEY"}, Validate: validText, Secret: true, Confirm: storageConfirm,
			Help: "The object store secret key"},
		{Name: HTTPProxy, Type: "url", Vars: []string{"HTTP_PROXY"}, Validate: validURL,
			Help: "The proxy Fluxer uses for outgoing HTTP requests"},
		{Name: HTTPSProxy, Type: "url", Vars: []string{"HTTPS_PROXY"}, Validate: validURL,
			Help: "The proxy Fluxer uses for outgoing HTTPS requests"},
		{Name: NoProxy, Type: "list", Vars: []string{"NO_PROXY"}, Validate: validText,
			Help: "Hosts Fluxer connects to without the proxy"},
		{Name: IPv6, Type: "bool", Validate: validBool, Confirm: "Every Fluxer service restarts.",
			Help: "Whether Fluxer's network uses IPv6"},
		{Name: SSOPrivate, Type: "bool", Vars: []string{"FLUXER_SSO_ALLOW_PRIVATE_ADDRESSES"}, Validate: validBool,
			Help: "Whether single sign-on can use a provider on a private or local network address"},
		{Name: ImagesRegistry, Type: "registry", Validate: validRegistry,
			Help: "A registry mirror for the database, cache, search, storage, voice and edge images"},
		{Name: MemoryProfile, Type: "choice", Options: []string{"small", "medium", "large"}, Validate: oneOf("small", "medium", "large"),
			Help: "A fixed amount of memory for Fluxer's services, instead of sizing them to this machine on every start"},
		{Name: EngineHost, Type: "endpoint", Validate: validEndpoint,
			Help: "The Docker or Podman engine that runs Fluxer"},
	}
	for _, svc := range slices.Sorted(maps.Keys(ImageVars)) {
		keys = append(keys, Key{Name: ImagesPrefix + svc, Type: "image", Vars: []string{ImageVars[svc]}, Validate: validImage,
			Help: "The image for " + svc})
	}
	for i := range keys {
		keys[i].Kind = Deployment
	}
	return keys
}

func Named() []Key {
	out := make([]Key, len(named))
	for i, k := range named {
		k.Vars = slices.Clone(k.Vars)
		k.Options = slices.Clone(k.Options)
		out[i] = k
	}
	return out
}

func namedKey(name string) (Key, bool) {
	for _, k := range named {
		if k.Name == name {
			return k, true
		}
	}
	return Key{}, false
}

func namedByVar(v string) (Key, bool) {
	for _, k := range named {
		if slices.Contains(k.Vars, v) {
			return k, true
		}
	}
	return Key{}, false
}

var (
	ErrUnknownKey   = errors.New("settings: unknown setting")
	ErrNeedsUpgrade = errors.New("settings: the release doesn't read this setting")
	ErrOtherName    = errors.New("settings: set under another name")
)

type Error struct {
	Key     string
	Use     string
	Problem string
	Fix     string
	Err     error
}

func (e *Error) Error() string { return e.Problem }

func (e *Error) Unwrap() error { return e.Err }

func Lookup(set *stack.Set, custom []string, name string, adminSchema []adminapi.ConfigKey) (Key, error) {
	if k, ok := namedKey(name); ok {
		if !releaseReads(set, custom, k) {
			return Key{}, &Error{Key: name, Err: ErrNeedsUpgrade,
				Problem: "This setting needs a newer Fluxer.", Fix: "Run rexctl upgrade first."}
		}
		return k, nil
	}
	for _, a := range adminSchema {
		if a.Key == name {
			return Key{Name: a.Key, Kind: Instance, Type: a.Type, Secret: a.SecretSet != "", Options: slices.Clone(a.Options), Help: a.Help}, nil
		}
	}
	if !isRawName(name) {
		return Key{}, unknown(name)
	}
	if k, ok := namedByVar(name); ok {
		return Key{}, otherName(name, k.Name)
	}
	if admin, ok := Twins[name]; ok {
		return Key{}, otherName(name, admin)
	}
	if !slices.Contains(set.Vars, name) && !slices.Contains(custom, name) {
		return Key{}, unknown(name)
	}
	return Key{Name: name, Kind: Raw, Type: "text", Vars: []string{name}, Secret: SecretName(name),
		Stateful: generatedStateful(name), Validate: validText}, nil
}

func unknown(name string) error {
	return &Error{Key: name, Err: ErrUnknownKey,
		Problem: fmt.Sprintf("rexctl doesn't know a setting called %s.", name),
		Fix:     "Run rexctl config list to see every setting."}
}

func otherName(name, use string) error {
	return &Error{Key: name, Use: use, Err: ErrOtherName,
		Problem: fmt.Sprintf("%s is set through %s.", name, use),
		Fix:     fmt.Sprintf("Use rexctl config set %s instead.", use)}
}

func releaseReads(set *stack.Set, custom []string, k Key) bool {
	if k.Name == MemoryProfile {
		return len(set.Profiles) > 0
	}
	for _, v := range k.Vars {
		if !slices.Contains(set.Vars, v) && !slices.Contains(custom, v) {
			return false
		}
	}
	return true
}

func isRawName(name string) bool {
	if name == "" || strings.ToUpper(name) != name {
		return false
	}
	for i, r := range name {
		switch {
		case r >= 'A' && r <= 'Z', r == '_':
		case r >= '0' && r <= '9' && i > 0:
		default:
			return false
		}
	}
	return true
}

func generatedStateful(name string) bool {
	for _, s := range secrets.Required {
		if s.Key == name {
			return s.Stateful
		}
	}
	return false
}
