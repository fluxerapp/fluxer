// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"maps"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type handRunValues struct {
	settings map[string]string
	secrets  map[string]string
	admin    map[string]string
	kept     int
	unknown  []string
	dropped  []string
}

var errListenConflict = errors.New("instance: the HTTP and HTTPS ports use different addresses")

var overlaySettings = map[stack.Overlay][2]string{
	stack.OverlayProxy:           {settings.EdgeMode, settings.EdgeProxy},
	stack.OverlayTunnel:          {settings.EdgeMode, settings.EdgeTunnel},
	stack.OverlayIPv6:            {settings.IPv6, "true"},
	stack.OverlayExternalVoice:   {settings.VoiceBundled, "false"},
	stack.OverlayKVPostgres:      {settings.KVMode, settings.KVPostgres},
	stack.OverlayFilesystemStore: {settings.StorageBackend, settings.StorageFilesystem},
	stack.OverlayRustServer:      {settings.RustProcess, settings.RustSingle},
}

func importHandRun(env map[string]string, set *stack.Set, custom []byte, overlays []stack.Overlay) (*handRunValues, error) {
	original := maps.Clone(env)
	r := &handRunValues{settings: map[string]string{}, secrets: map[string]string{}}
	read, err := settings.Vars(set, custom)
	if err != nil {
		return nil, err
	}
	defaults := composeDefaults(setFiles(set, custom))
	left := map[string]string{}
	for k, v := range env {
		switch {
		case strings.HasPrefix(k, "COMPOSE_"), k == "FLUXER_IMAGE_TAG" && !slices.Contains(read, k):
		case strings.TrimSpace(v) == "", equalsDefault(defaults[k], v, original):
		default:
			left[k] = v
		}
	}
	for _, s := range secrets.Required {
		if v, ok := left[s.Key]; ok {
			if !secrets.IsUnset(v) {
				r.secrets[s.Key] = v
			}
			delete(left, s.Key)
		}
	}
	if v, ok := left[SetupTokenVar]; ok {
		r.secrets[SetupTokenVar] = v
		delete(left, SetupTokenVar)
	}
	r.admin = twinsToAdmin(left, false)
	for _, p := range []struct{ key, name string }{{settings.HTTPProxy, "HTTP_PROXY"}, {settings.HTTPSProxy, "HTTPS_PROXY"}, {settings.NoProxy, "NO_PROXY"}} {
		v := left[p.name]
		if v == "" {
			v = left[strings.ToLower(p.name)]
		}
		delete(left, p.name)
		delete(left, strings.ToLower(p.name))
		if v != "" {
			r.settings[p.key] = v
		}
	}
	named := map[string]settings.Key{}
	for _, k := range settings.Named() {
		if len(k.Vars) > 0 {
			named[k.Vars[0]] = k
		}
	}
	external := slices.Contains(overlays, stack.OverlayExternalObjectStore)
	newVars := settings.NativeSet(stack.EmbeddedRelease()).Vars
	for _, k := range slices.Sorted(maps.Keys(left)) {
		v := left[k]
		key, isNamed := named[k]
		switch {
		case isNamed && (key.Name != settings.StorageEndpoint || external) && (key.Name != settings.VoicePublicIP || original["FLUXER_LIVEKIT_USE_EXTERNAL_IP"] == "false") && (key.Validate == nil || key.Validate(v) == nil):
			r.settings[key.Name] = v
		case slices.Contains(read, k):
			r.settings[k] = v
		case strings.HasPrefix(k, "FLUXER_") && slices.Contains(newVars, k):
			r.settings[k] = v
			r.unknown = append(r.unknown, k)
		default:
			r.dropped = append(r.dropped, k)
		}
	}
	for _, o := range overlays {
		if s, ok := overlaySettings[o]; ok {
			r.settings[s[0]] = s[1]
		}
	}
	var conflict *settings.ListenConflict
	if _, err := settings.SplitListenAddress(r.settings, nil); errors.As(err, &conflict) {
		return nil, errListenConflict
	}
	if err := keepEffective(set, r, original, read, defaults); err != nil {
		return nil, err
	}
	if err := dropUnused(set, r); err != nil {
		return nil, err
	}
	r.kept = len(r.settings) + len(r.secrets) + len(r.admin)
	return r, nil
}

func keepEffective(set *stack.Set, r *handRunValues, before map[string]string, read []string, defaults map[string][]string) error {
	for range 3 {
		after, err := settings.Effective(set, r.settings, r.secrets, r.admin)
		if err != nil {
			return err
		}
		changed := false
		for _, v := range read {
			if v == stack.SettingsLoadedVar || isGeneratedSecret(v) || settings.Twins[v] != "" {
				continue
			}
			b, a := before[v], after[v]
			if b == a || (b == "" && equalsDefault(defaults[v], a, after)) || (a == "" && equalsDefault(defaults[v], b, after)) {
				continue
			}
			changed = true
			r.settings[v] = b
		}
		if !changed {
			return nil
		}
	}
	return nil
}

func dropUnused(set *stack.Set, r *handRunValues) error {
	full, err := settings.Effective(set, r.settings, r.secrets, r.admin)
	if err != nil {
		return err
	}
	for _, k := range slices.Sorted(maps.Keys(r.settings)) {
		if !isRaw(k) || slices.Contains(r.unknown, k) {
			continue
		}
		v := r.settings[k]
		delete(r.settings, k)
		without, err := settings.Effective(set, r.settings, r.secrets, r.admin)
		if err != nil {
			return err
		}
		if !maps.Equal(without, full) {
			r.settings[k] = v
		}
	}
	return nil
}
