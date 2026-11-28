// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/compose-spec/compose-go/v2/dotenv"
	"github.com/compose-spec/compose-go/v2/template"
	"go.yaml.in/yaml/v4"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const overrideFileName = "compose.override.yaml"

var adoptKeys = []string{"image", "restart", "stop_grace_period", "env_file", "environment", "ports", "volumes", "sysctls", "extra_hosts"}

const EdgeModeVar = "FLUXER_EDGE_MODE"

var adoptDropped = []string{"FLUXER_INSTALL_METHOD", "FLUXER_UPGRADE_COMMAND"}

type PortLine struct {
	File  string
	Line  int
	Value string
}

type ComposeInstall struct {
	Dir        string
	Files      []string
	Project    string
	Service    string
	Volume     string
	Image      string
	Release    string
	Env        map[string]string
	Ports      []PortLine
	ExtraHosts []string
	Restart    string
	Problems   map[string][]string
	BadVolume  bool
	Multiline  string
}

func (c *ComposeInstall) problem(file string, line int, format string, args ...any) {
	if c.Problems == nil {
		c.Problems = map[string][]string{}
	}
	c.Problems[file] = append(c.Problems[file], fmt.Sprintf("line %d: ", line)+fmt.Sprintf(format, args...))
}

func ReadComposeInstall(dir string) (*ComposeInstall, error) {
	c := &ComposeInstall{Dir: dir, Files: []string{filepath.Join(dir, ComposeFileName)}}
	if _, err := os.Stat(filepath.Join(dir, overrideFileName)); err == nil {
		c.Files = append(c.Files, filepath.Join(dir, overrideFileName))
	}
	vars := map[string]string{}
	if f, err := envfile.Load(filepath.Join(dir, instance.EnvFileName)); err == nil {
		maps.Copy(vars, f.Map())
	}
	name := ""
	volumes := map[string]string{}
	var mount string
	for _, file := range c.Files {
		data, err := os.ReadFile(file)
		if err != nil {
			return nil, err
		}
		var doc yaml.Node
		if err := yaml.Unmarshal(data, &doc); err != nil {
			return nil, fmt.Errorf("%s: %w", filepath.Base(file), err)
		}
		if len(doc.Content) == 0 || doc.Content[0].Kind != yaml.MappingNode {
			continue
		}
		base := filepath.Base(file)
		top := doc.Content[0]
		for i := 0; i+1 < len(top.Content); i += 2 {
			k, v := top.Content[i], top.Content[i+1]
			switch k.Value {
			case "name":
				name = substitute(v.Value, vars)
			case "services":
				for j := 0; j+1 < len(v.Content); j += 2 {
					svcName, svc := v.Content[j], v.Content[j+1]
					if c.Service != "" && c.Service != svcName.Value {
						c.problem(base, svcName.Line, "%s has no rexctl setting.", svcName.Value)
						continue
					}
					c.Service = svcName.Value
					if src := c.readService(base, svc, vars); src != "" {
						mount = src
					}
				}
			case "volumes":
				for j := 0; j+1 < len(v.Content); j += 2 {
					vk, vv := v.Content[j], v.Content[j+1]
					volumes[vk.Value] = ""
					for n := 0; n+1 < len(vv.Content); n += 2 {
						switch vv.Content[n].Value {
						case "name":
							volumes[vk.Value] = substitute(vv.Content[n+1].Value, vars)
						default:
							c.problem(base, vv.Content[n].Line, "%s has no rexctl setting.", vv.Content[n].Value)
						}
					}
				}
			default:
				c.problem(base, k.Line, "%s has no rexctl setting.", k.Value)
			}
		}
	}
	if name == "" {
		name = strings.ToLower(filepath.Base(dir))
	}
	c.Project = name
	if mount != "" {
		c.Volume = volumes[mount]
		if c.Volume == "" {
			c.Volume = name + "_" + mount
		}
	}
	if err := c.readEnv(); err != nil {
		return nil, err
	}
	return c, nil
}

func substitute(s string, vars map[string]string) string {
	out, err := template.Substitute(s, func(k string) (string, bool) {
		if v, ok := os.LookupEnv(k); ok {
			return v, true
		}
		v, ok := vars[k]
		return v, ok
	})
	if err != nil {
		return s
	}
	return out
}

func (c *ComposeInstall) readService(file string, svc *yaml.Node, vars map[string]string) string {
	mount := ""
	for i := 0; i+1 < len(svc.Content); i += 2 {
		k, v := svc.Content[i], svc.Content[i+1]
		if !slices.Contains(adoptKeys, k.Value) {
			c.problem(file, k.Line, "%s has no rexctl setting.", k.Value)
			continue
		}
		switch k.Value {
		case "image":
			c.Image = substitute(v.Value, vars)
		case "restart":
			c.Restart = v.Value
		case "env_file":
			if !envFileIs(v, EnvFileName) {
				c.problem(file, k.Line, "%s has no rexctl setting.", k.Value)
			}
		case "environment":
			for _, key := range mapOrListKeys(v) {
				if key.Value != "FLUXER_INSTALL_METHOD" {
					c.problem(file, key.Line, "%s has no rexctl setting.", key.Value)
				}
			}
		case "sysctls":
			for _, key := range mapOrListKeys(v) {
				if key.Value != lowPortsSysctl {
					c.problem(file, key.Line, "%s has no rexctl setting.", key.Value)
				}
			}
		case "extra_hosts":
			for _, h := range v.Content {
				c.ExtraHosts = append(c.ExtraHosts, h.Value)
			}
		case "ports":
			for _, p := range v.Content {
				c.Ports = append(c.Ports, PortLine{File: file, Line: p.Line, Value: substitute(p.Value, vars)})
			}
		case "volumes":
			for _, vol := range v.Content {
				src, target := volumeParts(vol)
				switch {
				case target != ContainerData:
					entry := vol.Value
					if entry == "" {
						entry = src + ":" + target
					}
					c.problem(file, vol.Line, "%s has no rexctl setting.", entry)
				case src == "" || strings.ContainsAny(src[:1], "/.~"):
					c.problem(file, vol.Line, "Fluxer's data is in a folder, and rexctl keeps it in a volume.")
				default:
					mount = src
				}
			}
		}
	}
	return mount
}

func envFileIs(v *yaml.Node, name string) bool {
	switch v.Kind {
	case yaml.ScalarNode:
		return v.Value == name
	case yaml.SequenceNode:
		return len(v.Content) == 1 && v.Content[0].Kind == yaml.ScalarNode && v.Content[0].Value == name
	}
	return false
}

func mapOrListKeys(v *yaml.Node) []*yaml.Node {
	var out []*yaml.Node
	switch v.Kind {
	case yaml.MappingNode:
		for i := 0; i+1 < len(v.Content); i += 2 {
			out = append(out, v.Content[i])
		}
	case yaml.SequenceNode:
		for _, e := range v.Content {
			k, _, _ := strings.Cut(e.Value, "=")
			out = append(out, &yaml.Node{Kind: yaml.ScalarNode, Value: k, Line: e.Line})
		}
	}
	return out
}

func volumeParts(v *yaml.Node) (string, string) {
	if v.Kind == yaml.MappingNode {
		var src, target string
		for i := 0; i+1 < len(v.Content); i += 2 {
			switch v.Content[i].Value {
			case "source":
				src = v.Content[i+1].Value
			case "target":
				target = v.Content[i+1].Value
			}
		}
		return src, filepath.Clean(target)
	}
	parts := strings.Split(v.Value, ":")
	if len(parts) < 2 {
		return "", ""
	}
	return parts[0], filepath.Clean(parts[1])
}

func (c *ComposeInstall) readEnv() error {
	data, err := os.ReadFile(filepath.Join(c.Dir, EnvFileName))
	if err != nil {
		return err
	}
	env, err := dotenv.UnmarshalBytesWithLookup(data, func(string) (string, bool) { return "", false })
	if err != nil {
		return fmt.Errorf("%s: %w", EnvFileName, err)
	}
	for k, v := range env {
		if strings.ContainsAny(v, "\r\n") {
			c.Multiline = k
		}
		if slices.Contains(adoptDropped, k) {
			delete(env, k)
		}
	}
	c.Env = env
	return nil
}

func (c *ComposeInstall) ReleaseTag() (string, bool) {
	tag, ok := strings.CutPrefix(c.Image, ImageRepo+":")
	return tag, ok && release.Valid(tag)
}

func (c *ComposeInstall) InstanceName() (string, bool) {
	name, ok := strings.CutSuffix(c.Volume, volumeSuffix)
	return name, ok && name != ""
}

func (c *ComposeInstall) ForeignPorts(want []engine.PortMap) []PortLine {
	have := make([]string, len(want))
	for i, p := range want {
		have[i] = p.String()
	}
	var out []PortLine
	for _, p := range c.Ports {
		if !slices.Contains(have, normalPort(p.Value)) {
			out = append(out, p)
		}
	}
	return out
}

func normalPort(v string) string {
	return strings.TrimSuffix(strings.TrimSpace(v), "/tcp")
}

func PortFix(v string) (string, string) {
	spec, proto, _ := strings.Cut(normalPort(v), "/")
	parts := strings.Split(spec, ":")
	if len(parts) < 2 {
		return "", ""
	}
	host, inside := parts[len(parts)-2], parts[len(parts)-1]
	keys := map[string]string{"80": "FLUXER_HTTP_PORT", "443": "FLUXER_HTTPS_PORT", "7881": stack.VoiceTCPPortKey, "7882": stack.VoiceUDPPortKey}
	key, ok := keys[inside]
	if _, err := strconv.Atoi(host); !ok || err != nil {
		return "", ""
	}
	to := host + ":" + host
	if proto != "" {
		to += "/" + proto
	}
	return to, key + "=" + host
}

func ProxyBind(ports []PortLine) (string, bool) {
	for _, p := range ports {
		spec, _, _ := strings.Cut(normalPort(p.Value), "/")
		parts := strings.Split(spec, ":")
		if len(parts) < 2 || parts[len(parts)-1] != strconv.Itoa(containerProxy) {
			continue
		}
		host := "0.0.0.0"
		if len(parts) == 3 {
			host = parts[0]
		}
		return host + ":" + parts[len(parts)-2], true
	}
	return "", false
}

var ErrAdoptUnhealthy = errors.New("runtime: the compose container isn't healthy")

type Composed struct {
	Project *engine.Project
	Name    string
}

func FindComposed(ctx context.Context, e *engine.Engine, c *ComposeInstall) (Composed, error) {
	p, err := e.LoadPublished(ctx, engine.Spec{Name: c.Project, Dir: c.Dir, Files: c.Files, Env: composeVars(c.Dir)})
	if err != nil {
		return Composed{}, err
	}
	cs, err := p.PS(ctx)
	if err != nil {
		return Composed{}, err
	}
	for _, ct := range cs {
		if ct.Service == c.Service && ct.State == "running" && (ct.Health == "" || ct.Health == "healthy") {
			return Composed{Project: p, Name: ct.Name}, nil
		}
	}
	return Composed{Project: p}, ErrAdoptUnhealthy
}

func composeVars(dir string) map[string]string {
	f, err := envfile.Load(filepath.Join(dir, instance.EnvFileName))
	if err != nil {
		return nil
	}
	return f.Map()
}

func ReadComposedSecrets(ctx context.Context, e *engine.Engine, name string) (map[string]string, error) {
	return readComposedEnv(ctx, e, name, containerSecrets)
}

func ReadComposedModes(ctx context.Context, e *engine.Engine, name string) (map[string]string, error) {
	return readComposedEnv(ctx, e, name, containerModes)
}

func readComposedEnv(ctx context.Context, e *engine.Engine, name, file string) (map[string]string, error) {
	data, err := e.ReadSingleFile(ctx, name, ContainerData+"/"+file)
	if err != nil {
		return nil, err
	}
	f, err := envfile.Parse(file, data)
	if err != nil {
		return nil, err
	}
	return f.Map(), nil
}

func LiveBackup(ctx context.Context, e *engine.Engine, name string) error {
	return e.ExecSingle(ctx, name, engine.ExecOptions{}, "fluxerd", "backup", "--reason", "adopt")
}

func RemoveContainer(ctx context.Context, r Runtime) error {
	c, ok := r.(*containerRT)
	if !ok {
		return ErrUnsupported
	}
	e, err := c.engine(ctx)
	if err != nil {
		return err
	}
	bg := context.WithoutCancel(ctx)
	if err := e.StopSingle(bg, ContainerName(c.inst), ContainerStop); err != nil {
		return err
	}
	return e.RemoveSingle(bg, ContainerName(c.inst))
}
