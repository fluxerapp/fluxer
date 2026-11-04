// SPDX-License-Identifier: AGPL-3.0-or-later

package settings

import (
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestSmallDiskProfileKeepsUserValues(t *testing.T) {
	set := stack.Embedded()
	chosen := map[string]string{DiskProfile: stack.DiskProfileSmall, stack.LogMaxSizeVar: "20m"}
	env, err := Effective(set, chosen, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	if env[stack.LogMaxSizeVar] != "20m" {
		t.Fatalf("log size %q, want the user's 20m", env[stack.LogMaxSizeVar])
	}
	if env[stack.LogMaxFileVar] != "2" || env[stack.MaxWALSizeVar] != "512MB" || env[stack.SpoolTotalVar] != "536870912" {
		t.Fatalf("small profile values missing: %v %v %v", env[stack.LogMaxFileVar], env[stack.MaxWALSizeVar], env[stack.SpoolTotalVar])
	}

	plain, err := Effective(set, map[string]string{"FLUXER_MEDIA_PROXY_UPLOAD_RELAY_MAX_BODY_BYTES": "4294967296"}, nil, nil)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := plain[stack.LogMaxSizeVar]; ok {
		t.Fatalf("an instance without disk.profile got a log size of %q", plain[stack.LogMaxSizeVar])
	}
	if plain[stack.SpoolTotalVar] != "4294967296" {
		t.Fatalf("spool %q is smaller than the largest upload", plain[stack.SpoolTotalVar])
	}
	if stack.GrowthCap(set, env) >= stack.GrowthCap(set, plain) {
		t.Fatal("the small profile doesn't lower how much Fluxer's logs, WAL and spool can grow")
	}
}
