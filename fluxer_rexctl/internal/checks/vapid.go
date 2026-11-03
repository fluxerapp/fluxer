// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/secrets"
)

func init() {
	Register(Check{ID: "vapid", Phases: Installed, Run: runVAPID, Fix: fixVAPID})
}

func runVAPID(ctx context.Context, e *Env) Result {
	if e.state() == nil {
		return skip("")
	}
	err := secrets.ValidateVAPID(e.value(secrets.VAPIDPublicKey), e.value(secrets.VAPIDPrivateKey))
	switch {
	case err == nil:
		return pass("Browser notifications are set up.")
	case errors.Is(err, secrets.ErrVAPIDMissing):
		return fixable(fail("Browser notifications aren't set up.", "Run rexctl doctor --fix to set them up."))
	}
	return fixable(fail("Browser notifications don't work with the keys in your settings.",
		"Run rexctl doctor --fix to make new keys. Browsers sign up for notifications again on their own."))
}

func fixVAPID(ctx context.Context, e *Env) error {
	if e.state() == nil {
		return errNoProject
	}
	pair, err := secrets.GenerateVAPID()
	if err != nil {
		return err
	}
	e.stageSecret(secrets.VAPIDPublicKey, pair.Public)
	e.stageSecret(secrets.VAPIDPrivateKey, pair.Private)
	return nil
}
