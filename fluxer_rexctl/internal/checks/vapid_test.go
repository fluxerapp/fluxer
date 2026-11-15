// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"strings"
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
)

func TestFixVAPIDReplacesKeysSetAsSettings(t *testing.T) {
	ctx := context.Background()
	e := &Env{Instance: &instance.Instance{State: &instance.State{
		Settings: map[string]string{
			secrets.VAPIDPublicKey:  "junk-public",
			secrets.VAPIDPrivateKey: "junk-private",
		},
		Secrets: map[string]string{},
	}}}
	if r := runVAPID(ctx, e); r.Status != Fail {
		t.Fatalf("status before fix = %v, want fail", r.Status)
	}
	if err := fixVAPID(ctx, e); err != nil {
		t.Fatal(err)
	}
	if r := runVAPID(ctx, e); r.Status != Pass {
		t.Fatalf("status after fix = %v (%s), want pass", r.Status, r.Message)
	}
}

func TestFixVAPIDPlanUnsetsKeysSetAsSettings(t *testing.T) {
	e := &Env{Instance: &instance.Instance{State: &instance.State{
		Settings: map[string]string{secrets.VAPIDPrivateKey: "junk-private"},
		Secrets:  map[string]string{},
	}}}
	plan, err := PlanFix(context.Background(), e, "vapid")
	if err != nil {
		t.Fatal(err)
	}
	want := "rexctl config unset " + secrets.VAPIDPrivateKey
	for _, s := range plan.Steps {
		if strings.Contains(s.Undo, "junk-private") {
			t.Fatalf("plan step %+v shows the old private key", s)
		}
	}
	for _, s := range plan.Steps {
		if s.Command == want {
			return
		}
	}
	t.Fatalf("plan steps %+v do not include %q", plan.Steps, want)
}
