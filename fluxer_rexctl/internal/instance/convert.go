// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"maps"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func ConvertLegacy(s *State, next *stack.Set, nextImages map[string]string) map[string]string {
	pins := map[string]string{}
	for _, k := range legacyImageVars {
		delete(s.Settings, k)
	}
	infra := map[string]string{}
	for svc, v := range settings.ImageVars {
		infra[v] = svc
	}
	for _, k := range slices.Sorted(maps.Keys(s.Settings)) {
		if isRawVar(k) && s.Settings[k] == "" {
			delete(s.Settings, k)
			continue
		}
		if !isRawVar(k) || !isImageVar(k) {
			continue
		}
		v := s.Settings[k]
		delete(s.Settings, k)
		svc, ok := infra[k]
		if !ok || sameImage(v, nextImages[svc]) {
			continue
		}
		s.Settings[settings.ImagesPrefix+svc] = v
		pins[svc] = v
	}
	if v, ok := s.Settings[tunnelBindVar]; ok && s.Settings[settings.EdgeMode] == settings.EdgeTunnel && slices.Contains(next.Vars, edgeBindVar) {
		delete(s.Settings, tunnelBindVar)
		s.Settings[settings.EdgeBind] = v
	}
	stack.ConvertLegacySettings(s.Settings)
	return pins
}

const (
	tunnelBindVar = "FLUXER_HTTP_PORT"
	edgeBindVar   = "FLUXER_EDGE_BIND"
)

func sameImage(a, b string) bool {
	return a != "" && b != "" && imageName(a) == imageName(b)
}

func imageName(ref string) string {
	ref, _, _ = strings.Cut(strings.TrimSpace(ref), "@")
	ref = strings.TrimPrefix(ref, "docker.io/")
	ref = strings.TrimPrefix(ref, "library/")
	return ref
}
