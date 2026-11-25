// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"io"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

const ownBackupsDir = ContainerData + "/backups"

func FetchOwnBackups(ctx context.Context, r Runtime, dest string) ([]string, error) {
	if n, ok := r.(*nativeRT); ok {
		return n.n.FetchOwnBackups(dest)
	}
	c, ok := r.(*containerRT)
	if !ok {
		return nil, ErrUnsupported
	}
	e, err := c.engine(ctx)
	if err != nil {
		return nil, err
	}
	if err := os.MkdirAll(dest, 0o700); err != nil {
		return nil, err
	}
	var got []string
	err = e.CopyOut(ctx, c.name(), ownBackupsDir, func(name string, size int64, src io.Reader) error {
		base := path.Base(name)
		if strings.HasPrefix(base, ".") || !strings.HasSuffix(base, ".tar.zst") {
			return nil
		}
		target := filepath.Join(dest, base)
		if fi, err := os.Stat(target); err == nil && fi.Size() == size {
			return nil
		}
		tmp, err := os.CreateTemp(dest, ".fetch-*")
		if err != nil {
			return err
		}
		defer os.Remove(tmp.Name())
		if _, err := io.Copy(tmp, src); err != nil {
			tmp.Close()
			return err
		}
		if err := tmp.Close(); err != nil {
			return err
		}
		if err := os.Rename(tmp.Name(), target); err != nil {
			return err
		}
		got = append(got, target)
		return nil
	})
	if errors.Is(err, engine.ErrNoFile) {
		return nil, nil
	}
	return got, err
}
