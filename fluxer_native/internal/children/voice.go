// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"errors"
	"fmt"
	"io/fs"
	"net"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/config"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/edge"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	VoiceChildPrefix = "livekit-"
	voiceServersDir  = "voice"
	voiceEnvFile     = "livekit.env"
)

var voiceIDPattern = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{0,62}$`)

var voiceEnvKeys = []string{"FLUXER_VOICE_ID", "FLUXER_VOICE_HTTP_PORT", "GOMEMLIMIT", "LIVEKIT_KEYS", "LIVEKIT_CONFIG"}

func VoiceDir(c Config) string {
	return filepath.Join(filepath.Dir(c.CaddyDir), voiceServersDir)
}

func VoiceServers(c Config) ([]spec.Child, []error) {
	dir := VoiceDir(c)
	entries, err := os.ReadDir(dir)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, []error{fmt.Errorf("voice servers: %w", err)}
	}
	var ids []string
	for _, e := range entries {
		if e.IsDir() && voiceIDPattern.MatchString(e.Name()) {
			ids = append(ids, e.Name())
		}
	}
	slices.Sort(ids)
	var out []spec.Child
	var errs []error
	for _, id := range ids {
		ch, ok, err := c.voiceServer(filepath.Join(dir, id, voiceEnvFile), id)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if ok {
			out = append(out, ch)
		}
	}
	return out, errs
}

func (c Config) voiceServer(path, id string) (spec.Child, bool, error) {
	file, err := config.ReadEnvFile(path)
	if errors.Is(err, fs.ErrNotExist) {
		return spec.Child{}, false, nil
	}
	if err != nil {
		return spec.Child{}, false, fmt.Errorf("voice server %s: %w", id, err)
	}
	if file["FLUXER_VOICE_ID"] != id {
		return spec.Child{}, false, fmt.Errorf("voice server %s: %s is for voice server %q", id, path, file["FLUXER_VOICE_ID"])
	}
	port, err := strconv.Atoi(file["FLUXER_VOICE_HTTP_PORT"])
	if err != nil || port <= 0 || port > 65535 {
		return spec.Child{}, false, fmt.Errorf("voice server %s: FLUXER_VOICE_HTTP_PORT %q isn't a port", id, file["FLUXER_VOICE_HTTP_PORT"])
	}
	if file["LIVEKIT_KEYS"] == "" || file["LIVEKIT_CONFIG"] == "" {
		return spec.Child{}, false, fmt.Errorf("voice server %s: %s has no LiveKit keys or config", id, path)
	}
	probe := spec.HTTPProbe(port, "/", 200)
	if bind := strings.Trim(strings.TrimSpace(file["FLUXER_VOICE_HTTP_BIND"]), "[]"); bind != "" {
		ip := net.ParseIP(bind)
		if ip == nil {
			return spec.Child{}, false, fmt.Errorf("voice server %s: FLUXER_VOICE_HTTP_BIND %q isn't an IP address", id, bind)
		}
		if !ip.IsUnspecified() {
			probe.Addr = net.JoinHostPort(bind, strconv.Itoa(port))
		}
	}
	name := VoiceChildPrefix + id
	env := spec.Env{
		"PATH":   filepath.Join(c.Root, "bin") + ":" + systemPath,
		"HOME":   c.Layout.Dir(name),
		"TMPDIR": c.Layout.Tmp(name),
	}
	for _, k := range voiceEnvKeys {
		if v := file[k]; v != "" {
			env[k] = v
		}
	}
	return spec.Child{
		Name:        name,
		Argv:        []string{c.bin("livekit-server")},
		Env:         env,
		Cwd:         c.Layout.Dir(name),
		Probes:      []spec.Probe{probe},
		StopSignal:  syscall.SIGTERM,
		StopTimeout: 10 * time.Second,
	}, true, nil
}

func (c Config) VoiceOnly() bool {
	on, _ := strconv.ParseBool(strings.TrimSpace(c.Vars[spec.VoiceOnlyEnv]))
	return on
}

func CheckVoiceOnly(vars map[string]string) error {
	v := strings.TrimSpace(vars[spec.VoiceOnlyEnv])
	if v == "" {
		return nil
	}
	if _, err := strconv.ParseBool(v); err != nil {
		return fmt.Errorf("%s must be true or false, not %q", spec.VoiceOnlyEnv, v)
	}
	return nil
}

func voiceOnlySpecs(c Config) ([]spec.Child, error) {
	voices, _ := VoiceServers(c)
	errs := []error{CheckVoiceOnly(c.Vars), c.supervision(voices)}
	_, err := Edge(c)
	errs = append(errs, err)
	return voices, errors.Join(errs...)
}

func VoiceEdgeOff(vars map[string]string) bool {
	on, _ := strconv.ParseBool(strings.TrimSpace(vars[spec.VoiceOnlyEnv]))
	return on && strings.TrimSpace(vars["FLUXER_EDGE_MODE"]) == "none"
}

func (c Config) voiceEdge(e edge.Config) (edge.Config, error) {
	e.VoiceOnly = true
	voices, _ := VoiceServers(c)
	for _, v := range voices {
		e.Voices = append(e.Voices, edge.VoiceRoute{ID: strings.TrimPrefix(v.Name, VoiceChildPrefix), Addr: v.Probes[0].Addr})
	}
	switch mode := strings.TrimSpace(c.Vars["FLUXER_EDGE_MODE"]); mode {
	case "", "bundled":
		c.bundledTLS(&e)
	case "proxy":
		return e, c.proxyEdge(&e)
	case "none":
		e.Off = true
	default:
		return e, fmt.Errorf("FLUXER_EDGE_MODE must be bundled, proxy or none on a voice server, not %q", mode)
	}
	return e, nil
}
