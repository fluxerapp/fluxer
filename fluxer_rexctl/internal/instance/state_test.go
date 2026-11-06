// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"errors"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func TestParseStateSplitsListenAddress(t *testing.T) {
	data := []byte(`{"schema":5,"project":"fluxer","settings":{"edge.http_port":"192.168.24.35:80","edge.https_port":"192.168.24.35:443"}}`)
	s, err := ParseState(data, "/data/fluxer")
	if err != nil {
		t.Fatal(err)
	}
	if s.Settings[settings.EdgeListen] != "192.168.24.35" || s.Settings[settings.EdgeHTTPPort] != "80" || s.Settings[settings.EdgeHTTPSPort] != "443" || len(s.Repaired) != 2 {
		t.Fatalf("settings %v, repaired %v", s.Settings, s.Repaired)
	}

	differ := []byte(`{"schema":5,"project":"fluxer","settings":{"edge.http_port":"192.168.24.35:80","edge.https_port":"192.168.24.36:443"}}`)
	var se *SettingError
	if _, err := ParseState(differ, "/data/fluxer"); !errors.As(err, &se) || se.Remedy == "" {
		t.Fatalf("differing addresses: err %v", err)
	}
	repair := []string{settings.EdgeHTTPPort}
	if _, err := parseStateRepairing(differ, "/data/fluxer", repair); err != nil {
		t.Fatalf("config set is locked out: %v", err)
	}
}

func TestParseStateRefusesInvalidNames(t *testing.T) {
	if _, err := ParseState([]byte(`{"schema":5,"project":"Fluxer Prod"}`), "/data/fluxer"); !errors.Is(err, ErrBadProject) {
		t.Fatalf("project: err %v", err)
	}
	if _, err := ParseState([]byte(`{"schema":5,"project":"fluxer","voice":[{"id":"a/b"}]}`), "/data/fluxer"); err == nil {
		t.Fatal("voice server ID: no error")
	}
	if _, err := ParseState([]byte(`{"schema":5,"project":"fluxer","voice":[{"id":"EU.West_server-1"}]}`), "/data/fluxer"); err != nil {
		t.Fatalf("voice server ID: %v", err)
	}
}
