// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"maps"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func SendAdminImport(ctx context.Context, inst *instance.Instance, api *adminapi.Client) error {
	ai := inst.State.AdminImport
	if ai == nil || ai.SentAt != "" {
		return nil
	}
	if len(ai.Values) > 0 {
		schema, err := adminapi.ConfigSchema()
		if err != nil {
			return err
		}
		current, err := api.RawConfig(ctx)
		if err != nil {
			return err
		}
		patch := map[string]any{}
		for _, name := range slices.Sorted(maps.Keys(ai.Values)) {
			k, err := adminapi.FindConfigKey(schema, name)
			if err != nil || isSet(current, k) {
				continue
			}
			value, err := adminapi.ParseConfigValue(k, ai.Values[name])
			if err != nil {
				continue
			}
			adminapi.MergePatch(patch, adminapi.ConfigPatch(k.Key, value))
		}
		if len(patch) > 0 {
			if err := api.PatchConfig(ctx, patch); err != nil {
				return err
			}
		}
	}
	ai.SentAt = time.Now().UTC().Format(time.RFC3339)
	return save(inst)
}

func isSet(cfg map[string]any, k adminapi.ConfigKey) bool {
	path := k.Key
	if k.Type == "secret" {
		path = k.SecretSet
	}
	var cur any = cfg
	for _, p := range strings.Split(path, ".") {
		obj, ok := cur.(map[string]any)
		if !ok {
			return false
		}
		if cur, ok = obj[p]; !ok {
			return false
		}
	}
	switch v := cur.(type) {
	case nil:
		return false
	case bool:
		return k.Type != "secret" || v
	case string:
		return v != ""
	case []any:
		return len(v) > 0
	case map[string]any:
		return len(v) > 0
	}
	return true
}
