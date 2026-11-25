// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/migrate"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
)

func init() {
	Register(Check{ID: "uploads-read-only", Phases: Installed, Run: runUploadsReadOnly})
}

const openWritesCommand = "rexctl migrate --open-writes"

func runUploadsReadOnly(ctx context.Context, e *Env) Result {
	if e.Instance == nil {
		return skip("")
	}
	st, err := migrate.LoadState(e.Instance.Dir)
	if err != nil || st.Storage == nil || e.chosen(settings.StorageBackend) != settings.StorageFilesystem {
		return skip("")
	}
	if st.Storage.Open() {
		return pass("People can upload files again since the move off SeaweedFS.")
	}
	return warn("Uploads are still read-only after the move off SeaweedFS, so nobody can send files.", "Open a few channels and check that images and files load, then run "+openWritesCommand+".")
}
