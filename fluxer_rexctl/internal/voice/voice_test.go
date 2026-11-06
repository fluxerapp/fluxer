// SPDX-License-Identifier: AGPL-3.0-or-later

package voice

import (
	"bytes"
	"testing"

	"github.com/compose-spec/compose-go/v2/dotenv"
)

func TestEnvFileValuesStayOnOneLine(t *testing.T) {
	cases := map[string]Env{
		"plain":              {ID: "eu-server-1", Secret: "deadbeef", WebhookURL: "http://api:8080/webhooks/livekit?a=1&b=2"},
		"quoted turn":        {ID: "eu-server-1", TurnServers: `[{"host":"t","port":3478,"credential":"pa'ss$HOME #x"}]`},
		"multi-line webhook": {ID: "eu-server-1", WebhookURL: "http://api\nsecond line"},
		"trailing backslash": {ID: "eu-server-1", Secret: `ab c\`},
		"crlf":               {ID: "eu-server-1", Secret: "a\r\nb\"c\\d$e"},
	}
	for name, e := range cases {
		t.Run(name, func(t *testing.T) {
			want := e.Values()
			got, err := dotenv.ParseWithLookup(bytes.NewReader(e.File()), func(string) (string, bool) { return "", false })
			if err != nil {
				t.Fatal(err)
			}
			if len(got) != len(want) {
				t.Fatalf("parsed %d keys, want %d: %v", len(got), len(want), got)
			}
			for k, v := range want {
				if got[k] != v {
					t.Errorf("%s = %q, want %q", k, got[k], v)
				}
			}
		})
	}
}
