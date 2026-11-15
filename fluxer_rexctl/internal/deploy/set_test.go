// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
)

func vapidInstance(t *testing.T, public, private string) *instance.Instance {
	t.Helper()
	values := map[string]string{}
	if _, err := secrets.Mint(values, false); err != nil {
		t.Fatal(err)
	}
	return &instance.Instance{Dir: t.TempDir(), State: &instance.State{
		Settings: map[string]string{secrets.VAPIDPublicKey: public, secrets.VAPIDPrivateKey: private},
		Secrets:  values,
	}}
}

func TestPrepareUnsetsBrokenVAPIDSettings(t *testing.T) {
	inst := vapidInstance(t, "junk-public", "junk-private")
	pr, err := Prepare(inst, map[string]*string{secrets.VAPIDPublicKey: nil, secrets.VAPIDPrivateKey: nil})
	if err != nil {
		t.Fatal(err)
	}
	for _, key := range []string{secrets.VAPIDPublicKey, secrets.VAPIDPrivateKey} {
		if _, ok := pr.Settings[key]; ok {
			t.Fatalf("%s is still chosen after the unset", key)
		}
	}
}

func TestPrepareKeepsWorkingVAPIDSettings(t *testing.T) {
	pair, err := secrets.GenerateVAPID()
	if err != nil {
		t.Fatal(err)
	}
	inst := vapidInstance(t, pair.Public, pair.Private)
	if _, err := Prepare(inst, map[string]*string{secrets.VAPIDPublicKey: nil, secrets.VAPIDPrivateKey: nil}); err == nil {
		t.Fatal("Prepare changed a working VAPID pair")
	}
}

func TestPrepareSetsVAPIDOverBrokenSecrets(t *testing.T) {
	values := map[string]string{}
	if _, err := secrets.Mint(values, false); err != nil {
		t.Fatal(err)
	}
	values[secrets.VAPIDPublicKey] = "junk-public"
	values[secrets.VAPIDPrivateKey] = "junk-private"
	inst := &instance.Instance{Dir: t.TempDir(), State: &instance.State{Settings: map[string]string{}, Secrets: values}}
	pair, err := secrets.GenerateVAPID()
	if err != nil {
		t.Fatal(err)
	}
	pr, err := Prepare(inst, map[string]*string{secrets.VAPIDPublicKey: &pair.Public, secrets.VAPIDPrivateKey: &pair.Private})
	if err != nil {
		t.Fatal(err)
	}
	if pr.Settings[secrets.VAPIDPublicKey] != pair.Public || pr.Settings[secrets.VAPIDPrivateKey] != pair.Private {
		t.Fatal("the new VAPID pair was not chosen")
	}
}
