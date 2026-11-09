// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"context"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const ValkeyRetiredPrefix = "valkey.migrated-"

type OldData struct {
	Option Option
	What   string
	Ready  bool
	Wait   string
}

func OldDataOf(st State, newestBackup time.Time) []OldData {
	var out []OldData
	if s := st.KV; s.Pending() {
		d := OldData{Option: OptKV, What: stack.ValkeyVolume}
		if s.ValkeyDir != "" {
			d.What = s.ValkeyDir
		}
		d.Ready = newestBackup.After(s.At)
		if !d.Ready {
			d.Wait = "Make a backup with rexctl backup first, so the data now kept in Postgres is in one."
		}
		out = append(out, d)
	}
	if s := st.Storage; s.Pending() {
		d := OldData{Option: OptStorage, What: stack.SeaweedFSVolume}
		switch {
		case !s.Open():
			d.Wait = "Uploads are still read-only. Open them with rexctl migrate --experimental-native --open-writes first, or go back with rexctl migrate --experimental-native --undo storage."
		case !newestBackup.After(*s.OpenedAt):
			d.Wait = "Make a backup with rexctl backup first, so the files are in one."
		default:
			d.Ready = true
		}
		out = append(out, d)
	}
	return out
}

func CleanUp(ctx context.Context, h Host, st *State, d OldData, at time.Time) error {
	if !d.Ready {
		return ErrNoNewBackup
	}
	if err := h.DropData(ctx, d.What); err != nil {
		return err
	}
	at = at.UTC()
	switch d.Option {
	case OptKV:
		st.KV.CleanedUpAt = &at
	case OptStorage:
		st.Storage.CleanedUpAt = &at
	}
	return nil
}
