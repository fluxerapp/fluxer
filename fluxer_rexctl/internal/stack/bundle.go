// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
)

const BundleProject = "fluxer"

func Bundle(dir string, images map[string]string) error {
	set := Embedded()
	files, _, err := renderFiles(set, nil, images, BundleProject)
	if err != nil {
		return err
	}
	for _, o := range overlayOrder {
		data, err := ReadFile(set, OverlayFiles[o])
		if err != nil {
			return err
		}
		if o == OverlayRustServer {
			if data, err = renderRustOverlay(data, [][]byte{files[0].data}); err != nil {
				return err
			}
		}
		if set.Release != LegacyRelease {
			if data, err = pinImages(data, set.Release, images); err != nil {
				return err
			}
		}
		files = append(files, renderedFile{OverlayFiles[o], data})
	}
	for _, d := range madeDirs {
		if err := makeBundleDir(filepath.Join(dir, d)); err != nil {
			return err
		}
	}
	for _, f := range files {
		if err := envfile.WriteFileAtomic(filepath.Join(dir, f.name), f.data, false); err != nil {
			return err
		}
	}
	return fs.WalkDir(set.Files, "voice", func(name string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		target := filepath.Join(dir, filepath.FromSlash(name))
		if d.IsDir() {
			return makeBundleDir(target)
		}
		data, err := ReadFile(set, name)
		if err != nil {
			return err
		}
		if strings.HasSuffix(name, ".compose.yml") && set.Release != LegacyRelease {
			if data, err = pinImages(data, set.Release, images); err != nil {
				return err
			}
		}
		return envfile.WriteFileAtomic(target, data, false)
	})
}

func makeBundleDir(path string) error {
	if err := os.MkdirAll(path, 0o755); err != nil {
		return err
	}
	info, err := os.Lstat(path)
	if err != nil {
		return err
	}
	if !info.IsDir() {
		return errors.New("stack: " + path + " is not a plain folder")
	}
	return nil
}
