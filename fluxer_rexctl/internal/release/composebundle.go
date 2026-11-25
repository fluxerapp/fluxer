// SPDX-License-Identifier: AGPL-3.0-or-later

package release

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
)

const (
	ReleasesAPI    = "https://api.github.com/repos/fluxerapp/fluxer/releases/tags/fluxer-release@"
	ReleasesAPIEnv = "REXCTL_RELEASES_API"
	maxBundleBytes = 64 << 20
)

var (
	ErrNoBundle      = errors.New("release: no compose bundle for this version")
	ErrBundleHash    = errors.New("release: the compose bundle doesn't match its published SHA-256")
	ErrBundleContent = errors.New("release: the compose bundle has an entry outside its folder")
)

type githubRelease struct {
	Assets []struct {
		Name   string `json:"name"`
		URL    string `json:"browser_download_url"`
		Digest string `json:"digest"`
	} `json:"assets"`
}

func releasesAPI() string {
	if u := os.Getenv(ReleasesAPIEnv); u != "" {
		return u
	}
	return ReleasesAPI
}

func ComposeBundleName(version string) string { return "fluxer-compose-" + version + ".tar.gz" }

func FetchComposeBundle(ctx context.Context, version, dir string) error {
	if !Valid(version) {
		return fmt.Errorf("%w: %q", ErrNoBundle, version)
	}
	var rel githubRelease
	if err := getJSON(ctx, releasesAPI()+version, &rel); err != nil {
		return err
	}
	name := ComposeBundleName(version)
	for _, a := range rel.Assets {
		if a.Name != name {
			continue
		}
		want, ok := strings.CutPrefix(a.Digest, "sha256:")
		if !ok || len(want) != sha256.Size*2 {
			return fmt.Errorf("%w: %s has no SHA-256 on its release", ErrBundleHash, name)
		}
		return fetchBundle(ctx, a.URL, want, dir)
	}
	return fmt.Errorf("%w: %s", ErrNoBundle, version)
}

func getJSON(ctx context.Context, url string, v any) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return err
	}
	req.Header.Set("Accept", "application/vnd.github+json")
	req.Header.Set("User-Agent", "rexctl")
	resp, err := httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode == http.StatusNotFound {
		return ErrNoBundle
	}
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("%w: status %d", ErrBadResponse, resp.StatusCode)
	}
	return json.NewDecoder(io.LimitReader(resp.Body, 4<<20)).Decode(v)
}

func fetchBundle(ctx context.Context, url, want, dir string) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return err
	}
	req.Header.Set("User-Agent", "rexctl")
	resp, err := httpClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("%w: status %d", ErrBadResponse, resp.StatusCode)
	}
	data, err := io.ReadAll(io.LimitReader(resp.Body, maxBundleBytes))
	if err != nil {
		return err
	}
	sum := sha256.Sum256(data)
	if hex.EncodeToString(sum[:]) != strings.ToLower(want) {
		return ErrBundleHash
	}
	if err := os.MkdirAll(filepath.Dir(dir), 0o755); err != nil {
		return err
	}
	tmp, err := os.MkdirTemp(filepath.Dir(dir), filepath.Base(dir)+".download-")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmp)
	if err := os.Chmod(tmp, 0o755); err != nil {
		return err
	}
	if err := extractBundle(data, tmp); err != nil {
		return err
	}
	if err := os.Rename(tmp, dir); err != nil {
		if info, serr := os.Stat(dir); serr == nil && info.IsDir() {
			return nil
		}
		return err
	}
	return nil
}

func extractBundle(data []byte, dir string) error {
	gz, err := gzip.NewReader(bytes.NewReader(data))
	if err != nil {
		return err
	}
	tr := tar.NewReader(gz)
	for {
		hdr, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return err
		}
		name := filepath.Clean(filepath.FromSlash(hdr.Name))
		if name == "." {
			continue
		}
		if filepath.IsAbs(name) || name == ".." || strings.HasPrefix(name, ".."+string(filepath.Separator)) {
			return fmt.Errorf("%w: %s", ErrBundleContent, hdr.Name)
		}
		target := filepath.Join(dir, name)
		switch hdr.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(target, 0o755); err != nil {
				return err
			}
		case tar.TypeReg:
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				return err
			}
			f, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
			if err != nil {
				return err
			}
			_, err = io.Copy(f, io.LimitReader(tr, maxBundleBytes))
			if cerr := f.Close(); err == nil {
				err = cerr
			}
			if err != nil {
				return err
			}
		default:
			return fmt.Errorf("%w: %s", ErrBundleContent, hdr.Name)
		}
	}
}
