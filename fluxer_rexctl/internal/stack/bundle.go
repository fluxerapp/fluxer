// SPDX-License-Identifier: AGPL-3.0-or-later

package stack

import (
	"os"
	"path/filepath"
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
	for _, d := range MountedDirs {
		if err := os.MkdirAll(filepath.Join(dir, d), 0o755); err != nil {
			return err
		}
	}
	for _, f := range files {
		if err := os.WriteFile(filepath.Join(dir, f.name), f.data, 0o644); err != nil {
			return err
		}
	}
	return nil
}
