// SPDX-License-Identifier: AGPL-3.0-or-later

package stamp

import (
	"cmp"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

const FileName = "VERSION"

type Action int

const (
	Keep Action = iota
	Adopt
	Upgrade
	Refuse
)

func Path(dataDir string) string { return filepath.Join(dataDir, FileName) }

func Read(dataDir string) (string, error) {
	data, err := os.ReadFile(Path(dataDir))
	if errors.Is(err, fs.ErrNotExist) {
		return "", nil
	}
	return strings.TrimSpace(string(data)), err
}

func Write(dataDir, version string) error {
	if !Valid(version) {
		return Remove(dataDir)
	}
	tmp, err := os.CreateTemp(dataDir, ".version-*")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(version + "\n"); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Chmod(0o644); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Sync(); err != nil {
		tmp.Close()
		return err
	}
	if err := tmp.Close(); err != nil {
		return err
	}
	return os.Rename(tmp.Name(), Path(dataDir))
}

func Remove(dataDir string) error {
	if err := os.Remove(Path(dataDir)); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return nil
}

func Decide(stamped, release string) Action {
	if !Valid(release) {
		return Keep
	}
	if !Valid(stamped) {
		return Adopt
	}
	switch c, _ := Compare(release, stamped); {
	case c > 0:
		return Upgrade
	case c < 0:
		return Refuse
	}
	return Keep
}

func Valid(v string) bool {
	_, ok := parse(v)
	return ok
}

func Compare(a, b string) (int, bool) {
	pa, ok := parse(a)
	if !ok {
		return 0, false
	}
	pb, ok := parse(b)
	if !ok {
		return 0, false
	}
	for i := range pa {
		if c := cmp.Compare(pa[i], pb[i]); c != 0 {
			return c, true
		}
	}
	return 0, true
}

func parse(v string) ([3]uint64, bool) {
	var out [3]uint64
	fields := strings.Split(v, ".")
	if len(fields) != len(out) {
		return out, false
	}
	for i, f := range fields {
		if f == "" || strings.Trim(f, "0123456789") != "" {
			return out, false
		}
		n, err := strconv.ParseUint(f, 10, 64)
		if err != nil {
			return out, false
		}
		out[i] = n
	}
	return out, true
}
