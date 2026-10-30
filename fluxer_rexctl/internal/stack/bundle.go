// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
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
		files = append(files, renderedFile{OverlayFiles[o], data})
	}
	for _, d := range madeDirs {
		if err := os.MkdirAll(filepath.Join(dir, d), 0o755); err != nil {
			return err
		}
	}
	for _, f := range files {
		if err := os.WriteFile(filepath.Join(dir, f.name), f.data, 0o644); err != nil {
			return err
		}
	}
	return fs.WalkDir(set.Files, "voice", func(name string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		target := filepath.Join(dir, filepath.FromSlash(name))
		if d.IsDir() {
			return os.MkdirAll(target, 0o755)
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
		return os.WriteFile(target, data, 0o644)
	})
}
