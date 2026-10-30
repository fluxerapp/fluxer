// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"fmt"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func Interrupted(inst *instance.Instance) string {
	p := inst.State.Pending
	if p == nil {
		return ""
	}
	if len(p.Keys) == 1 {
		return fmt.Sprintf("The change to %s didn't finish. rexctl put the old value back.", p.Keys[0])
	}
	if len(p.Keys) == 0 {
		return "The last settings change didn't finish. rexctl put the old values back."
	}
	return fmt.Sprintf("The change to %s and %s didn't finish. rexctl put the old values back.",
		strings.Join(p.Keys[:len(p.Keys)-1], ", "), p.Keys[len(p.Keys)-1])
}

func Resolve(ctx context.Context, inst *instance.Instance, e *engine.Engine) error {
	p := inst.State.Pending
	if p == nil {
		return nil
	}
	rs, err := ReleaseSet(inst)
	if err != nil {
		return err
	}
	ipv6 := parseBool(p.Settings[settings.IPv6]) != parseBool(inst.State.Settings[settings.IPv6])
	if _, err := render(inst, rs, inst.State.Settings); err != nil {
		return err
	}
	if err := save(inst); err != nil {
		return err
	}
	var detached []AttachedVoice
	if ipv6 {
		if detached, err = detachVoice(ctx, e, inst); err != nil {
			return err
		}
		if err := downInstance(ctx, e, inst); err != nil {
			return err
		}
	}
	if !inst.State.Stopped {
		s, err := specFor(inst, rs, inst.State.Settings)
		if err != nil {
			return err
		}
		proj, err := e.Load(ctx, s)
		if err != nil {
			return err
		}
		if err := up(ctx, proj, true, nil); err != nil {
			return err
		}
	}
	inst.State.Pending = nil
	if err := save(inst); err != nil {
		return err
	}
	if inst.State.Stopped {
		return nil
	}
	if failed := attachVoice(ctx, e, detached); len(failed) > 0 {
		return fmt.Errorf("deploy: voice servers %s didn't start again", strings.Join(failed, ", "))
	}
	return nil
}
