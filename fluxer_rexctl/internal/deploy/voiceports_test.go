// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func TestCheckPortsFreeSeesStoppedVoiceServers(t *testing.T) {
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_CONFIG_HOME", filepath.Join(home, ".config"))
	inst := &instance.Instance{Dir: filepath.Join(home, "voice"), State: &instance.State{
		Schema: instance.StateSchema, Kind: instance.KindVoice, Project: "fluxer-voice",
		Settings: map[string]string{}, Secrets: map[string]string{},
		Voice: []instance.VoiceServer{{ID: "fra-server-1", TCPPort: 47883, UDPPort: 47884}},
	}}
	if err := os.MkdirAll(inst.Path(instance.StateDirName), 0o700); err != nil {
		t.Fatal(err)
	}
	if err := inst.Save(); err != nil {
		t.Fatal(err)
	}
	if err := instance.Remember(instance.OwnScope(), inst.Dir, false); err != nil {
		t.Fatal(err)
	}
	err := checkPortsFree(context.Background(), nil, "fluxer", []host.Port{{Number: 47883, Proto: "tcp"}})
	if err == nil || !strings.Contains(err.Error(), "fra-server-1") {
		t.Fatalf("checkPortsFree = %v", err)
	}
	if err := checkPortsFree(context.Background(), nil, "fluxer", []host.Port{{Number: 47885, Proto: "tcp"}}); err != nil {
		t.Fatalf("a free port: %v", err)
	}
}
