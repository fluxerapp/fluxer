// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
)

func adoptSecrets(ctx context.Context, inst *instance.Instance, r rt.Runtime) error {
	if r.Kind() == rt.Container {
		restored, err := rt.RestoredSecrets(ctx, r)
		if err != nil {
			return err
		}
		return adoptRestored(inst, restored)
	}
	return adoptNativeSecrets(inst, native.New(inst))
}

func adoptNativeSecrets(inst *instance.Instance, n *native.Stack) error {
	restored, err := n.ReadSecrets()
	if err != nil {
		return err
	}
	return adoptRestored(inst, restored)
}

func adoptRestored(inst *instance.Instance, restored map[string]string) error {
	keys := []string{setup.TokenKey}
	for _, s := range secrets.Required {
		keys = append(keys, s.Key)
	}
	for _, k := range keys {
		if v, ok := restored[k]; ok && v != "" {
			inst.State.Secrets[k] = v
		}
	}
	return inst.Save()
}
