// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"archive/tar"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"time"

	"github.com/klauspost/compress/zstd"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
)

var (
	ErrChecksum  = errors.New("native: the download doesn't match its checksum")
	ErrNoFluxerd = errors.New("native: the native build has no " + FluxerdPath)
	ErrBadEntry  = errors.New("native: the native build has an unsafe path")
)

var fetchClient = &http.Client{Timeout: 30 * time.Minute}

func (s *Stack) Has(version string) bool {
	info, err := os.Stat(filepath.Join(s.ReleaseDir(version), FluxerdPath))
	return err == nil && !info.IsDir()
}

func (s *Stack) Fetch(ctx context.Context, rel *release.Release, progress func(cur, total int64)) (string, error) {
	if !release.Valid(rel.Version) {
		return "", fmt.Errorf("native: %q isn't a release version", rel.Version)
	}
	dir := s.ReleaseDir(rel.Version)
	if s.Has(rel.Version) {
		return dir, nil
	}
	asset, err := rel.DistFor(runtime.GOARCH)
	if err != nil {
		return "", err
	}
	if err := os.MkdirAll(s.ReleasesDir(), 0o755); err != nil {
		return "", err
	}
	part := filepath.Join(s.ReleasesDir(), "."+rel.Version+".download")
	defer os.Remove(part)
	if err := download(ctx, asset, part, progress); err != nil {
		return "", err
	}
	tmp := filepath.Join(s.ReleasesDir(), "."+rel.Version+".unpack")
	_ = os.RemoveAll(tmp)
	defer os.RemoveAll(tmp)
	if err := Unpack(part, tmp); err != nil {
		return "", err
	}
	if info, err := os.Stat(filepath.Join(tmp, FluxerdPath)); err != nil || info.IsDir() {
		return "", ErrNoFluxerd
	}
	_ = os.RemoveAll(dir)
	if err := os.Rename(tmp, dir); err != nil {
		return "", err
	}
	return dir, nil
}

func download(ctx context.Context, asset release.Asset, path string, progress func(cur, total int64)) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, asset.URL, nil)
	if err != nil {
		return err
	}
	req.Header.Set("User-Agent", "rexctl")
	resp, err := fetchClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("native: downloading %s: status %d", asset.URL, resp.StatusCode)
	}
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return err
	}
	h := sha256.New()
	var r io.Reader = resp.Body
	if progress != nil {
		r = &counter{r: r, total: resp.ContentLength, progress: progress}
	}
	if _, err := io.Copy(io.MultiWriter(f, h), r); err != nil {
		f.Close()
		return err
	}
	if err := f.Close(); err != nil {
		return err
	}
	if !strings.EqualFold(hex.EncodeToString(h.Sum(nil)), asset.SHA256) {
		return ErrChecksum
	}
	return nil
}

type counter struct {
	r        io.Reader
	cur      int64
	total    int64
	progress func(cur, total int64)
}

func (c *counter) Read(p []byte) (int, error) {
	n, err := c.r.Read(p)
	c.cur += int64(n)
	c.progress(c.cur, c.total)
	return n, err
}

func Unpack(archive, dest string) error {
	f, err := os.Open(archive)
	if err != nil {
		return err
	}
	defer f.Close()
	zr, err := zstd.NewReader(f)
	if err != nil {
		return err
	}
	defer zr.Close()
	if err := os.MkdirAll(dest, 0o755); err != nil {
		return err
	}
	root, err := os.OpenRoot(dest)
	if err != nil {
		return err
	}
	defer root.Close()
	tr := tar.NewReader(zr)
	for {
		hdr, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return err
		}
		if err := unpackEntry(root, tr, hdr); err != nil {
			return err
		}
	}
}

func inside(path string) bool {
	return path != ".." && !strings.HasPrefix(path, ".."+string(filepath.Separator)) && !filepath.IsAbs(path)
}

func unpackEntry(root *os.Root, tr *tar.Reader, hdr *tar.Header) error {
	name := filepath.Clean(filepath.FromSlash(hdr.Name))
	if name == "." && hdr.Typeflag == tar.TypeDir {
		return nil
	}
	if name == "." || !inside(name) {
		return fmt.Errorf("%w: %s", ErrBadEntry, hdr.Name)
	}
	if err := root.MkdirAll(filepath.Dir(name), 0o755); err != nil {
		return err
	}
	mode := os.FileMode(hdr.Mode).Perm() &^ 0o022
	switch hdr.Typeflag {
	case tar.TypeDir:
		return root.MkdirAll(name, mode|0o700)
	case tar.TypeReg:
		out, err := root.OpenFile(name, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, mode)
		if err != nil {
			return err
		}
		if _, err := io.Copy(out, tr); err != nil {
			out.Close()
			return err
		}
		return out.Close()
	case tar.TypeSymlink:
		link := filepath.FromSlash(hdr.Linkname)
		if filepath.IsAbs(link) || !inside(filepath.Join(filepath.Dir(name), link)) {
			return fmt.Errorf("%w: %s -> %s", ErrBadEntry, hdr.Name, hdr.Linkname)
		}
		return root.Symlink(hdr.Linkname, name)
	case tar.TypeLink:
		src := filepath.Clean(filepath.FromSlash(hdr.Linkname))
		if !inside(src) {
			return fmt.Errorf("%w: %s -> %s", ErrBadEntry, hdr.Name, hdr.Linkname)
		}
		return root.Link(src, name)
	}
	return fmt.Errorf("%w: %s has type %c", ErrBadEntry, hdr.Name, hdr.Typeflag)
}

func linkTarget(path string) string {
	target, err := os.Readlink(path)
	if err != nil {
		return ""
	}
	return filepath.Base(target)
}

func (s *Stack) Running() string { return linkTarget(s.Current()) }

func HoldsReleases(dir string) bool {
	for _, name := range []string{ReleasesDirName, CurrentLink, PreviousLink} {
		if _, err := os.Lstat(filepath.Join(dir, name)); err == nil {
			return true
		}
	}
	return false
}

func (s *Stack) previous() string { return s.Inst.Path(PreviousLink) }

func (s *Stack) Flip(version string) error {
	if !s.Has(version) {
		return fmt.Errorf("native: Fluxer %s isn't unpacked in %s", version, s.ReleasesDir())
	}
	if old := s.Running(); old != "" && old != version {
		if err := relink(s.previous(), old); err != nil {
			return err
		}
	}
	return relink(s.Current(), version)
}

func relink(path, version string) error {
	tmp := path + ".new"
	_ = os.Remove(tmp)
	if err := os.Symlink(filepath.Join(ReleasesDirName, version), tmp); err != nil {
		return err
	}
	return os.Rename(tmp, path)
}

func (s *Stack) GoBack() (string, error) {
	prev := linkTarget(s.previous())
	if prev == "" || !s.Has(prev) {
		return "", fmt.Errorf("native: there's no earlier Fluxer build in %s", s.ReleasesDir())
	}
	if err := os.Rename(s.previous(), s.Current()); err != nil {
		return "", err
	}
	return prev, nil
}

func (s *Stack) Prune() error {
	entries, err := os.ReadDir(s.ReleasesDir())
	if err != nil {
		return err
	}
	keep := []string{s.Running(), linkTarget(s.previous())}
	for _, e := range entries {
		name := e.Name()
		if strings.HasPrefix(name, ".") || slices.Contains(keep, name) {
			continue
		}
		if err := os.RemoveAll(filepath.Join(s.ReleasesDir(), name)); err != nil {
			return err
		}
	}
	return nil
}
