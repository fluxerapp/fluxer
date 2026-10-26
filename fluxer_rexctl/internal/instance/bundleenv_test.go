// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"maps"
	"regexp"
	"slices"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestBundleEnvKeys(t *testing.T) {
	out := BundleEnv()
	if out != BundleEnv() {
		t.Fatal("BundleEnv is not deterministic")
	}
	f, err := envfile.Parse(".env.example", []byte(out))
	if err != nil {
		t.Fatal(err)
	}

	wantActive := map[string]string{stack.SettingsLoadedVar: "true", "FLUXER_DOMAIN": "", SetupTokenVar: ""}
	for _, s := range secrets.Required {
		wantActive[s.Key] = ""
		if s.Kind == secrets.KindFixed {
			wantActive[s.Key] = s.Fixed
		}
	}
	if got := f.Map(); !maps.Equal(got, wantActive) {
		t.Errorf("active keys\n got %v\nwant %v", got, wantActive)
	}

	var wantCommented []string
	for _, k := range settings.Named() {
		for _, v := range k.Vars {
			if _, ok := wantActive[v]; !ok && slices.Contains(stack.Embedded().Vars, v) {
				wantCommented = append(wantCommented, v)
			}
		}
	}
	var gotCommented []string
	for _, m := range regexp.MustCompile(`(?m)^# ([A-Z][A-Z0-9_]*)=$`).FindAllStringSubmatch(out, -1) {
		gotCommented = append(gotCommented, m[1])
	}
	slices.Sort(wantCommented)
	slices.Sort(gotCommented)
	if !slices.Equal(gotCommented, wantCommented) {
		t.Errorf("commented keys\n got %v\nwant %v", gotCommented, wantCommented)
	}
}
