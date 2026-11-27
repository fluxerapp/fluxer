// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"cmp"
	"errors"
	"fmt"
	"io"
	"maps"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/compose-spec/compose-go/v2/types"
	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type customProblem struct {
	line  int
	where string
	fix   string
}

type customReview struct {
	project   string
	dir       string
	chosen    map[string]string
	stock     *types.Project
	live      *types.Project
	stockWith func(map[string]string) (*types.Project, error)
	storage   bool
	problems  []customProblem
	portsLine int
}

func (r *customReview) add(line int, where, fix string) {
	r.problems = append(r.problems, customProblem{line: line, where: where, fix: fix})
}

func (r *customReview) refusal() error {
	if len(r.problems) == 0 {
		return nil
	}
	slices.SortStableFunc(r.problems, func(a, b customProblem) int { return a.line - b.line })
	tail := make([]string, len(r.problems))
	for i, p := range r.problems {
		tail[i] = fmt.Sprintf("Line %d, %s: %s", p.line, p.where, p.fix)
	}
	return &ui.ExitError{
		Code: ui.ExitRefused,
		Msg:  fmt.Sprintf("%s has %s the new layout can't take over.", instance.CustomCompose, oneOrMany(len(r.problems), "a line", "lines")),
		Fix:  "Change what the list below says with rexctl config edit --compose, then run " + migrateCommand + " again.",
		Tail: tail,
		Err:  errors.New(strings.Join(tail, "\n")),
	}
}

func oneOrMany(n int, one, many string) string {
	if n == 1 {
		return one
	}
	return many
}

func (r *customReview) read(custom []byte) error {
	dec := yaml.NewDecoder(bytes.NewReader(custom))
	for {
		var doc yaml.Node
		err := dec.Decode(&doc)
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return err
		}
		if len(doc.Content) == 0 {
			continue
		}
		root := doc.Content[0]
		if root.Kind == yaml.ScalarNode && root.Tag == "!!null" {
			continue
		}
		if root.Kind != yaml.MappingNode {
			r.add(root.Line, "the file", "rexctl expects services and volumes here. Remove it.")
			continue
		}
		for i := 0; i+1 < len(root.Content); i += 2 {
			k, v := root.Content[i], root.Content[i+1]
			switch {
			case k.Value == "volumes":
				r.volumePins(v)
			case k.Value == "services":
				r.services(v)
			case strings.HasPrefix(k.Value, "x-"):
			default:
				r.add(k.Line, k.Value, "the new layout has no Compose "+k.Value+". Remove it.")
			}
		}
	}
}

func (r *customReview) volumePins(v *yaml.Node) {
	if v.Kind == yaml.ScalarNode && v.Tag == "!!null" {
		return
	}
	if v.Kind != yaml.MappingNode {
		r.add(v.Line, "volumes", "rexctl expects a list of volumes here. Remove it.")
		return
	}
	for i := 0; i+1 < len(v.Content); i += 2 {
		k, def := v.Content[i], v.Content[i+1]
		var decoded any
		if err := def.Decode(&decoded); err == nil && volumePin(k.Value, decoded, r.project) {
			continue
		}
		fix := "the new layout has no volume called " + k.Value + ". Remove it."
		if _, ok := stack.DataMounts[k.Value]; ok {
			fix = "the move reads this volume only as " + r.project + "_" + k.Value + ". Keep just external: true and that name, or remove it."
		}
		r.add(k.Line, "volumes."+k.Value, fix)
	}
}

func (r *customReview) services(v *yaml.Node) {
	if v.Kind != yaml.MappingNode {
		if v.Kind != yaml.ScalarNode || v.Tag != "!!null" {
			r.add(v.Line, "services", "rexctl expects a list of services here. Remove it.")
		}
		return
	}
	for i := 0; i+1 < len(v.Content); i += 2 {
		name, body := v.Content[i].Value, v.Content[i+1]
		if body.Kind == yaml.ScalarNode && body.Tag == "!!null" {
			continue
		}
		if body.Kind != yaml.MappingNode {
			r.add(body.Line, "services."+name, "rexctl expects the settings of "+name+" here. Remove it.")
			continue
		}
		for j := 0; j+1 < len(body.Content); j += 2 {
			k, field := body.Content[j], body.Content[j+1]
			where := "services." + name + "." + k.Value
			switch {
			case k.Value == "volumes":
				r.serviceVolumes(name, where, field)
			case k.Value == "ports" && name == "edge":
				r.portsLine = k.Line
			case k.Value == "environment":
				r.add(k.Line, where, "the new layout runs every part of Fluxer with one set of settings. Set these with rexctl config set, then remove them here.")
			default:
				r.add(k.Line, where, "the new layout has no separate "+name+" container for this. Set what it does with rexctl config set if a setting covers it, then remove it.")
			}
		}
	}
}

func (r *customReview) serviceVolumes(svc, where string, v *yaml.Node) {
	if v.Tag == "!reset" {
		r.add(v.Line, where, "!reset drops the folders Fluxer keeps its data in. Remove it.")
		return
	}
	if v.Kind != yaml.SequenceNode {
		r.add(v.Line, where, "rexctl expects a list of mounts here. Remove it.")
		return
	}
	objects := false
	for _, item := range v.Content {
		src, dst, ro, ok := volumeEntry(item)
		if !ok {
			r.add(item.Line, where, "rexctl can't read this mount. Write it as ./folder:/path in the container.")
			continue
		}
		objects = objects || dst == stack.ObjectsRoot
		if fix := r.mountProblem(svc, src, dst, ro); fix != "" {
			r.add(item.Line, where+" "+src+":"+dst, fix)
		}
	}
	if r.storage && v.Tag == "!override" && !objects && slices.Contains(storageUsers, svc) {
		r.add(v.Line, where, "the move off SeaweedFS adds a volume for the uploaded files here, and !override would drop it. Remove !override, so the mounts listed here add to Fluxer's own.")
	}
}

func volumeEntry(n *yaml.Node) (src, dst string, ro, ok bool) {
	switch n.Kind {
	case yaml.ScalarNode:
		parts := strings.Split(n.Value, ":")
		if len(parts) < 2 || len(parts) > 3 || parts[0] == "" || parts[1] == "" {
			return "", "", false, false
		}
		if len(parts) == 3 {
			ro = slices.Contains(strings.Split(parts[2], ","), "ro")
		}
		return parts[0], parts[1], ro, true
	case yaml.MappingNode:
		var m struct {
			Type     string `yaml:"type"`
			Source   string `yaml:"source"`
			Target   string `yaml:"target"`
			ReadOnly bool   `yaml:"read_only"`
		}
		if err := n.Decode(&m); err != nil || m.Source == "" || m.Target == "" {
			return "", "", false, false
		}
		if m.Type == "bind" && !bindSource(m.Source) {
			m.Source = "./" + m.Source
		}
		return m.Source, m.Target, m.ReadOnly, true
	}
	return "", "", false, false
}

func bindSource(src string) bool {
	return src == "." || src == "~" || strings.HasPrefix(src, "/") || strings.HasPrefix(src, "./") || strings.HasPrefix(src, "../") || strings.HasPrefix(src, "~/")
}

func (r *customReview) hostPath(src string) string {
	if filepath.IsAbs(src) || strings.HasPrefix(src, "~") {
		return cleanPath(src)
	}
	return cleanPath(filepath.Join(r.dir, src))
}

func (r *customReview) mountProblem(svc, src, dst string, ro bool) string {
	bind := bindSource(src)
	place, data := placeAt(svc, dst)
	switch {
	case data && bind, data && src == place.short:
		return ""
	case svc == "media-proxy" && dst == "/tmp":
		return ""
	case r.stockMount(svc, src, dst, bind):
		return ""
	case svc == "edge" && dst == caddyfileTarget && bind:
		if r.stockCaddyfile(src) {
			return ""
		}
		return "the new layout writes its own Caddy configuration. Put what you added to " + src + " in a .caddy file in " + instance.CaddyDropInDir + ", then remove the line."
	case (svc == "api" || svc == "worker") && bind && ro && geoIPUnder(r.geoIPPath(), dst):
		return ""
	case (svc == "api" || svc == "worker") && bind && ro:
		return "nothing in Fluxer reads from " + dst + ". If the folder holds the GeoIP database, run rexctl config set " + settings.GeoIPDatabase + "=" + dst + "/<file>.mmdb first. Otherwise remove the line."
	case data:
		return "the move reads the data of " + svc + " only from a folder on this machine or from the " + place.short + " volume. Mount a folder here, or remove the line."
	}
	return "the new layout has nothing at " + dst + " to mount this on. Remove the line."
}

func (r *customReview) stockMount(svc, src, dst string, bind bool) bool {
	s, ok := modelService(r.stock, svc)
	if !ok {
		return false
	}
	for _, v := range s.Volumes {
		if v.Target != dst {
			continue
		}
		if bind {
			return v.Type == types.VolumeTypeBind && cleanPath(v.Source) == r.hostPath(src)
		}
		return v.Type == types.VolumeTypeVolume && v.Source == src
	}
	return false
}

const caddyfileTarget = "/etc/caddy/Caddyfile"

func (r *customReview) stockCaddyfile(src string) bool {
	s, ok := modelService(r.stock, "edge")
	if !ok {
		return false
	}
	for _, v := range s.Volumes {
		if v.Target != caddyfileTarget || v.Type != types.VolumeTypeBind {
			continue
		}
		stock, err := os.ReadFile(v.Source)
		if err != nil {
			return false
		}
		own, err := os.ReadFile(r.hostPath(src))
		return err == nil && bytes.Equal(own, stock)
	}
	return false
}

func (r *customReview) geoIPPath() string {
	return serviceEnv(r.live, "api", settings.GeoIPVar)
}

func serviceEnv(m *types.Project, svc, name string) string {
	s, ok := modelService(m, svc)
	if !ok {
		return ""
	}
	if v := s.Environment[name]; v != nil {
		return strings.TrimSpace(*v)
	}
	return ""
}

type portKey struct {
	ip, published string
	target        uint32
	proto         string
}

func edgeListens(mode string) []portKey {
	switch mode {
	case settings.EdgeProxy:
		return []portKey{{target: 8080, proto: "tcp"}}
	case settings.EdgeTunnel:
		return []portKey{{target: 80, proto: "tcp"}}
	}
	return []portKey{{target: 80, proto: "tcp"}, {target: 443, proto: "tcp"}, {target: 443, proto: "udp"}}
}

func listenedPorts(m *types.Project, mode string) []portKey {
	svc, ok := modelService(m, "edge")
	if !ok {
		return nil
	}
	listens := edgeListens(mode)
	var out []portKey
	for _, p := range svc.Ports {
		k := portKey{ip: p.HostIP, published: p.Published, target: p.Target, proto: cmp.Or(strings.ToLower(p.Protocol), "tcp")}
		if k.ip == "0.0.0.0" {
			k.ip = ""
		}
		if slices.ContainsFunc(listens, func(l portKey) bool { return l.target == k.target && l.proto == k.proto }) && !slices.Contains(out, k) {
			out = append(out, k)
		}
	}
	slices.SortFunc(out, func(a, b portKey) int {
		return cmp.Or(cmp.Compare(a.target, b.target), strings.Compare(a.proto, b.proto), strings.Compare(a.ip, b.ip), strings.Compare(a.published, b.published))
	})
	return out
}

func (r *customReview) ports() {
	if r.portsLine == 0 {
		return
	}
	mode := cmp.Or(strings.TrimSpace(r.chosen[settings.EdgeMode]), settings.EdgeBundled)
	live := listenedPorts(r.live, mode)
	if slices.Equal(live, listenedPorts(r.stock, mode)) {
		return
	}
	const where = "services.edge.ports"
	cannot := "the new layout can't listen like this. Remove the line, and set edge.bind, edge.listen_address, edge.http_port or edge.https_port with rexctl config set instead."
	changes, ok := portSettings(mode, live)
	if !ok {
		r.add(r.portsLine, where, cannot)
		return
	}
	for k, v := range changes {
		if key, found := settingKey(k); found && key.Validate != nil && key.Validate(v) != nil {
			r.add(r.portsLine, where, cannot)
			return
		}
	}
	m, err := r.stockWith(changes)
	if err != nil || !slices.Equal(live, listenedPorts(m, mode)) {
		r.add(r.portsLine, where, cannot)
		return
	}
	var set []string
	for _, k := range slices.Sorted(maps.Keys(changes)) {
		if cmp.Or(strings.TrimSpace(r.chosen[k]), portDefaults[k]) != changes[k] {
			set = append(set, k+"="+changes[k])
		}
	}
	if len(set) == 0 {
		return
	}
	r.add(r.portsLine, where, "run rexctl config set "+strings.Join(set, " ")+" first, so the new layout listens on the same "+oneOrMany(len(live), "port", "ports")+".")
}

var portDefaults = map[string]string{settings.EdgeHTTPPort: "80", settings.EdgeHTTPSPort: "443"}

func settingKey(name string) (settings.Key, bool) {
	for _, k := range settings.Named() {
		if k.Name == name {
			return k, true
		}
	}
	return settings.Key{}, false
}

func portSettings(mode string, live []portKey) (map[string]string, bool) {
	if mode != settings.EdgeBundled {
		if len(live) != 1 {
			return nil, false
		}
		return map[string]string{settings.EdgeBind: net.JoinHostPort(cmp.Or(live[0].ip, "0.0.0.0"), live[0].published)}, true
	}
	out := map[string]string{}
	ip := ""
	for i, p := range live {
		if i > 0 && p.ip != ip {
			return nil, false
		}
		ip = p.ip
		switch {
		case p.target == 80:
			out[settings.EdgeHTTPPort] = p.published
		case p.proto == "tcp":
			out[settings.EdgeHTTPSPort] = p.published
		}
	}
	if out[settings.EdgeHTTPPort] == "" || out[settings.EdgeHTTPSPort] == "" {
		return nil, false
	}
	if ip != "" {
		out[settings.EdgeListen] = ip
	}
	return out, true
}
