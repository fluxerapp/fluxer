// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/sizing"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

func TestGatewayEnv(t *testing.T) {
	data := t.TempDir()
	c := Config{
		Layout: spec.Layout{Data: data, Runtime: t.TempDir()},
		Root:   "/opt/fluxer/current",
		Vars:   map[string]string{"FLUXER_DOMAIN": "chat.example.com", "FLUXER_ERLANG_COOKIE": "cookie"},
		Host:   sizing.Host{CPUs: 8},
	}
	env, err := c.env(spec.Gateway)
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]string{
		"FLUXER_GATEWAY_BIND_HOST":           spec.Loopback,
		"FLUXER_GATEWAY_PORT":                strconv.Itoa(spec.PortGateway),
		"FLUXER_GATEWAY_STATUS_PORT":         "0",
		"FLUXER_ERLANG_SCHEDULERS":           "8",
		"FLUXER_ERLANG_DIRTY_CPU_SCHEDULERS": "4",
		"FLUXER_ERLANG_DIRTY_IO_SCHEDULERS":  "4",
		"ERL_CRASH_DUMP":                     filepath.Join(data, "gateway", "erl_crash.dump"),
		"LANG":                               "C.UTF-8",
		"ERL_INETRC":                         filepath.Join("/opt/fluxer/current", "gateway", "config", "inetrc"),
		"HOME":                               filepath.Join(data, "gateway"),
	}
	for k, v := range want {
		if env[k] != v {
			t.Errorf("%s = %q, want %q", k, env[k], v)
		}
	}
	for k := range env {
		if strings.HasPrefix(k, "RELX_") || k == "VMARGS_PATH" {
			t.Errorf("%s is set, the gateway launcher does not read relx settings", k)
		}
	}
}

func TestGatewayStatusPortIgnoresOverride(t *testing.T) {
	c := Config{
		Layout: spec.Layout{Data: t.TempDir(), Runtime: t.TempDir()},
		Root:   "/opt/fluxer/current",
		Vars:   map[string]string{"FLUXER_NATIVE_GATEWAY__FLUXER_GATEWAY_STATUS_PORT": strconv.Itoa(spec.PortMediaProxy)},
		Host:   sizing.Host{CPUs: 2},
	}
	env, err := c.env(spec.Gateway)
	if err != nil {
		t.Fatal(err)
	}
	if env["FLUXER_GATEWAY_STATUS_PORT"] != "0" {
		t.Fatalf("FLUXER_GATEWAY_STATUS_PORT = %q, want 0 so the gateway opens no second listener", env["FLUXER_GATEWAY_STATUS_PORT"])
	}
}
