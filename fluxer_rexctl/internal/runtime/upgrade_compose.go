// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var errNoComposeUpgrades = errors.New("runtime: no compose upgrade driver is set")

type composeUpgrades struct{ c *compose }

func (u composeUpgrades) Current(ctx context.Context) (string, error) {
	return u.c.inst.State.Release, nil
}

func (composeUpgrades) Check(context.Context, *release.Release) error { return errNoComposeUpgrades }

func (composeUpgrades) Prepare(context.Context, *ui.Step, *release.Release) error {
	return errNoComposeUpgrades
}

func (composeUpgrades) Switch(context.Context, *ui.Step, *release.Release) error {
	return errNoComposeUpgrades
}

func (composeUpgrades) StartVerify(context.Context, *ui.Step) error { return errNoComposeUpgrades }

func (composeUpgrades) GoBack(context.Context, *ui.Step, string) error { return errNoComposeUpgrades }

func (composeUpgrades) Prune(context.Context, []string) error { return errNoComposeUpgrades }
