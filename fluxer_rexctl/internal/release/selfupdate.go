// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"archive/tar"
	"archive/zip"
	"bytes"
	"compress/gzip"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/creativeprojects/go-selfupdate/update"
)

const maxArchiveBytes = 256 << 20

var (
	ErrChecksum           = errors.New("release: the download doesn't match its checksum")
	ErrNoBinary           = errors.New("release: the archive has no rexctl binary")
	ErrBadAsset           = errors.New("release: the asset has no URL or checksum")
	selfPath, selfPathErr = executablePath()
)

func executablePath() (string, error) {
	p, err := os.Executable()
	if err != nil {
		return "", err
	}
	return filepath.EvalSymlinks(p)
}

func Executable() (string, error) { return selfPath, selfPathErr }

func RemoveOldBinary() {
	if selfPathErr != nil {
		return
	}
	_ = os.Remove(filepath.Join(filepath.Dir(selfPath), "."+filepath.Base(selfPath)+".old"))
}

func SelfUpdate(ctx context.Context, a Asset) error {
	if selfPathErr != nil {
		return selfPathErr
	}
	want, err := hex.DecodeString(strings.TrimSpace(a.SHA256))
	if err != nil || len(want) != sha256.Size || a.URL == "" {
		return ErrBadAsset
	}
	archive, err := download(ctx, a.URL)
	if err != nil {
		return err
	}
	sum := sha256.Sum256(archive)
	if !bytes.Equal(sum[:], want) {
		return ErrChecksum
	}
	bin, err := extract(a.URL, archive)
	if err != nil {
		return err
	}
	binSum := sha256.Sum256(bin)
	RemoveOldBinary()
	return update.Apply(bytes.NewReader(bin), update.Options{TargetPath: selfPath, Checksum: binSum[:]})
}

func download(ctx context.Context, rawURL string) ([]byte, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, rawURL, nil)
	if err != nil {
		return nil, err
	}
	req.Header.Set("User-Agent", "rexctl")
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("%w: status %d", ErrBadResponse, resp.StatusCode)
	}
	data, err := io.ReadAll(io.LimitReader(resp.Body, maxArchiveBytes+1))
	if err != nil {
		return nil, err
	}
	if len(data) > maxArchiveBytes {
		return nil, fmt.Errorf("%w: the download is too large", ErrBadResponse)
	}
	return data, nil
}

func binaryName() string {
	if runtime.GOOS == "windows" {
		return "rexctl.exe"
	}
	return "rexctl"
}

func extract(rawURL string, archive []byte) ([]byte, error) {
	name := strings.ToLower(rawURL)
	if i := strings.IndexAny(name, "?#"); i >= 0 {
		name = name[:i]
	}
	if strings.HasSuffix(name, ".zip") {
		return extractZip(archive)
	}
	return extractTarGz(archive)
}

func extractTarGz(archive []byte) ([]byte, error) {
	gz, err := gzip.NewReader(bytes.NewReader(archive))
	if err != nil {
		return nil, err
	}
	defer gz.Close()
	tr := tar.NewReader(gz)
	for {
		h, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return nil, ErrNoBinary
		}
		if err != nil {
			return nil, err
		}
		if h.Typeflag != tar.TypeReg || path.Base(h.Name) != binaryName() {
			continue
		}
		return io.ReadAll(io.LimitReader(tr, maxArchiveBytes))
	}
}

func extractZip(archive []byte) ([]byte, error) {
	zr, err := zip.NewReader(bytes.NewReader(archive), int64(len(archive)))
	if err != nil {
		return nil, err
	}
	for _, f := range zr.File {
		if f.FileInfo().IsDir() || path.Base(f.Name) != binaryName() {
			continue
		}
		rc, err := f.Open()
		if err != nil {
			return nil, err
		}
		defer rc.Close()
		return io.ReadAll(io.LimitReader(rc, maxArchiveBytes))
	}
	return nil, ErrNoBinary
}
