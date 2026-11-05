// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"encoding/base64"
	"maps"
	"slices"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

func TestSigningSecretsKeepsExistingKeys(t *testing.T) {
	const own = "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8="
	rs := stack.Embedded()
	enforce := map[string]string{settings.Domain: "chat.example.com", settings.SignedMedia: "enforce"}
	with := func(extra map[string]string) map[string]string {
		out := maps.Clone(enforce)
		maps.Copy(out, extra)
		return out
	}

	st := &instance.State{Secrets: map[string]string{}}
	got, err := signingSecrets(rs, st, with(nil))
	if err != nil {
		t.Fatal(err)
	}
	key, err := base64.StdEncoding.DecodeString(got[settings.SignatureSecretsVar])
	if err != nil || len(key) != 32 {
		t.Fatalf("generated key is %d bytes, err %v", len(key), err)
	}
	if len(st.Secrets) != 0 {
		t.Fatal("signingSecrets changed the saved secrets")
	}

	st = &instance.State{Secrets: map[string]string{settings.SignatureSecretsVar: own}}
	if got, err := signingSecrets(rs, st, with(nil)); err != nil || got[settings.SignatureSecretsVar] != own {
		t.Fatalf("generated key replaced: got %q, err %v", got[settings.SignatureSecretsVar], err)
	}

	st = &instance.State{Secrets: map[string]string{}}
	if got, err := signingSecrets(rs, st, with(map[string]string{settings.SignatureSecretsVar: own})); err != nil || got[settings.SignatureSecretsVar] != "" {
		t.Fatalf("operator key shadowed by a generated one: got %q, err %v", got[settings.SignatureSecretsVar], err)
	}

	if _, err := signingSecrets(rs, st, with(map[string]string{settings.SignatureSecretsVar: ""})); err == nil {
		t.Fatal("an empty operator key with enforce was accepted")
	}

	off := with(map[string]string{settings.SignedMedia: "off"})
	if got, err := signingSecrets(rs, st, off); err != nil || got[settings.SignatureSecretsVar] != "" {
		t.Fatalf("off made a key: got %q, err %v", got[settings.SignatureSecretsVar], err)
	}
}

func TestListenAddressRestartsEdge(t *testing.T) {
	rs := stack.Embedded()
	k, err := Lookup(rs, nil, settings.EdgeListen)
	if err != nil {
		t.Fatal(err)
	}
	if got := Promised(rs, nil, k); !slices.Equal(got, []string{"edge"}) {
		t.Fatalf("Promised = %v, want edge", got)
	}
}
