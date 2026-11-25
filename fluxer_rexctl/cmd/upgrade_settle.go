// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const settleFlag = "settle"

func settleRelease(ctx context.Context, app *App) error {
	inst, err := app.Instance()
	if err != nil {
		return err
	}
	l, err := inst.Lock(ctx)
	if errors.Is(err, instance.ErrLocked) {
		return nil
	}
	if err != nil {
		return err
	}
	defer l.Release()
	eng, err := upgradeEngine(ctx, app, inst, nil)
	if err != nil {
		return err
	}
	target, moved, err := eng.Settle()
	if err != nil {
		return ui.Wrap(err, "Couldn't point Fluxer at "+fluxerName(target)+".", "Run rexctl upgrade --verbose to see why.")
	}
	if !moved {
		return nil
	}
	app.UI.Info("Fluxer starts %s, the release rexctl last recorded.", fluxerName(target))
	if inst.State.Upgrade != nil {
		app.UI.Info("Run rexctl upgrade to finish the %s.", pendingWhat(inst.State.Upgrade))
	}
	return nil
}
