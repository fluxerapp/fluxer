// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"os"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func nativeBackup(ctx context.Context, app *App, inst *instance.Instance, n *native.Stack, set *stack.Set, title, reason string) (string, error) {
	var path string
	err := app.UI.Step(ctx, title, func(ctx context.Context, s *ui.Step) error {
		var err error
		if path, err = n.Backup(ctx, set, inst.BackupDir(), reason, detailWriter{app.UI}); err != nil {
			return rt.NativeError(err, "Couldn't back up your instance.")
		}
		size := int64(0)
		if info, err := os.Stat(path); err == nil {
			size = info.Size()
		}
		s.Done("Backed up your instance (%s).", ui.Bytes(size))
		return nil
	})
	return path, err
}
