// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"testing"

	"github.com/compose-spec/compose-go/v2/template"
	"go.yaml.in/yaml/v4"
)

type livekitTURNServer struct {
	Host       string `yaml:"host"`
	Port       int    `yaml:"port"`
	Protocol   string `yaml:"protocol"`
	Username   string `yaml:"username"`
	Credential string `yaml:"credential"`
}

type livekitConfig struct {
	RTC struct {
		UseExternalIP bool                `yaml:"use_external_ip"`
		STUNServers   []string            `yaml:"stun_servers"`
		TURNServers   []livekitTURNServer `yaml:"turn_servers"`
	} `yaml:"rtc"`
}

func renderLivekitConfig(t *testing.T, env map[string]string) livekitConfig {
	t.Helper()
	data, err := ReadFile(Embedded(), "docker-compose.yml")
	if err != nil {
		t.Fatal(err)
	}
	var compose struct {
		Services map[string]struct {
			Environment map[string]string `yaml:"environment"`
		} `yaml:"services"`
	}
	if err := yaml.Unmarshal(data, &compose); err != nil {
		t.Fatal(err)
	}
	raw, ok := compose.Services["livekit"].Environment["LIVEKIT_CONFIG"]
	if !ok {
		t.Fatal("livekit service has no LIVEKIT_CONFIG")
	}
	rendered, err := template.Substitute(raw, func(k string) (string, bool) {
		v, ok := env[k]
		return v, ok
	})
	if err != nil {
		t.Fatal(err)
	}
	var cfg livekitConfig
	if err := yaml.Unmarshal([]byte(rendered), &cfg); err != nil {
		t.Fatalf("LIVEKIT_CONFIG is not valid YAML: %v\n%s", err, rendered)
	}
	return cfg
}

func TestLivekitConfigTURNServersFromEnv(t *testing.T) {
	cfg := renderLivekitConfig(t, map[string]string{
		"FLUXER_LIVEKIT_TURN_SERVERS": `[{"host":"turn.example.com","port":3478,"protocol":"udp","username":"u","credential":"p"}]`,
	})
	want := []livekitTURNServer{{Host: "turn.example.com", Port: 3478, Protocol: "udp", Username: "u", Credential: "p"}}
	if len(cfg.RTC.TURNServers) != 1 || cfg.RTC.TURNServers[0] != want[0] {
		t.Errorf("rtc.turn_servers = %+v, want %+v", cfg.RTC.TURNServers, want)
	}
	if !cfg.RTC.UseExternalIP || len(cfg.RTC.STUNServers) != 2 {
		t.Errorf("rtc = %+v, want use_external_ip and two STUN servers kept", cfg.RTC)
	}
}

func TestLivekitConfigNoTURNServersByDefault(t *testing.T) {
	cfg := renderLivekitConfig(t, nil)
	if len(cfg.RTC.TURNServers) != 0 {
		t.Errorf("rtc.turn_servers = %+v, want none", cfg.RTC.TURNServers)
	}
}
