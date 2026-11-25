// SPDX-License-Identifier: AGPL-3.0-or-later

package pg

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

const MinMajor = 14

const LibRoot = "/usr/lib/postgresql"

type Install struct {
	Major  int
	BinDir string
}

type Choice struct {
	Install
	Newer int
}

func BinDir(major int) string {
	return filepath.Join(LibRoot, strconv.Itoa(major), "bin")
}

func Installed(root, override string) []Install {
	if override != "" {
		major, _ := binMajor(override)
		return []Install{{Major: major, BinDir: override}}
	}
	entries, err := os.ReadDir(root)
	if err != nil {
		return nil
	}
	var out []Install
	for _, e := range entries {
		major, err := strconv.Atoi(e.Name())
		if err != nil || major <= 0 {
			continue
		}
		bin := filepath.Join(root, e.Name(), "bin")
		if info, err := os.Stat(filepath.Join(bin, "postgres")); err == nil && info.Mode().IsRegular() {
			out = append(out, Install{Major: major, BinDir: bin})
		}
	}
	slices.SortFunc(out, func(a, b Install) int { return b.Major - a.Major })
	return out
}

var versionLine = regexp.MustCompile(`\(PostgreSQL\) (\d+)`)

func binMajor(binDir string) (int, bool) {
	out, err := exec.Command(filepath.Join(binDir, "postgres"), "--version").Output()
	if err != nil {
		return 0, false
	}
	return parseServerVersion(string(out))
}

func parseServerVersion(s string) (int, bool) {
	if m := versionLine.FindStringSubmatch(s); m != nil {
		s = m[1]
	}
	digits := strings.TrimSpace(s)
	end := strings.IndexFunc(digits, func(r rune) bool { return r < '0' || r > '9' })
	if end >= 0 {
		digits = digits[:end]
	}
	n, err := strconv.Atoi(digits)
	return n, err == nil && n > 0
}

func DataMajor(dataDir string) (int, bool, error) {
	data, err := os.ReadFile(filepath.Join(dataDir, "PG_VERSION"))
	if errors.Is(err, fs.ErrNotExist) {
		return 0, false, nil
	}
	if err != nil {
		return 0, false, err
	}
	major, ok := parseServerVersion(string(data))
	if !ok {
		return 0, false, fmt.Errorf("postgres: %s/PG_VERSION holds %q, not a PostgreSQL version", dataDir, strings.TrimSpace(string(data)))
	}
	return major, true, nil
}

func Resolve(dataDir, override string) (Choice, error) {
	if err := FinishUpgrade(dataDir); err != nil {
		return Choice{}, err
	}
	major, ok, err := DataMajor(dataDir)
	if err != nil {
		return Choice{}, err
	}
	if ok && Linked(dataDir) {
		return Choice{}, &BrokenError{DataDir: dataDir, Major: major}
	}
	return Choose(dataDir, major, ok, Installed(LibRoot, override))
}

func Choose(dataDir string, dataMajor int, hasData bool, installed []Install) (Choice, error) {
	if len(installed) == 1 && installed[0].Major == 0 {
		return Choice{Install: installed[0]}, nil
	}
	newest := 0
	if len(installed) > 0 {
		newest = installed[0].Major
	}
	if !hasData {
		switch {
		case newest == 0:
			return Choice{}, errors.New("postgres: PostgreSQL isn't installed. Run sudo apt install postgresql, or sudo rexctl install")
		case newest < MinMajor:
			return Choice{}, fmt.Errorf("postgres: PostgreSQL %d is too old, Fluxer needs %d or later. Install a newer postgresql package from apt.postgresql.org", newest, MinMajor)
		}
		return Choice{Install: installed[0]}, nil
	}
	if dataMajor < MinMajor {
		return Choice{}, fmt.Errorf("postgres: the database in %s is PostgreSQL %d, and Fluxer needs %d or later. Restore your latest backup into a new install", dataDir, dataMajor, MinMajor)
	}
	for _, in := range installed {
		if in.Major == dataMajor {
			c := Choice{Install: in}
			if newest > dataMajor {
				c.Newer = newest
			}
			return c, nil
		}
	}
	have := "no PostgreSQL is installed"
	switch {
	case len(installed) == 1:
		have = fmt.Sprintf("only PostgreSQL %d is installed", newest)
	case len(installed) > 1:
		have = fmt.Sprintf("only PostgreSQL %s are installed", joinMajors(installed))
	}
	return Choice{}, &MissingError{DataDir: dataDir, Major: dataMajor, Have: have}
}

type MissingError struct {
	DataDir string
	Major   int
	Have    string
}

func (e *MissingError) Error() string {
	return fmt.Sprintf("postgres: the database in %s is PostgreSQL %d, and %s. "+
		"Install postgresql-%d (from apt.postgresql.org if this release no longer has it), then run sudo rexctl postgres upgrade, or fluxerd postgres-upgrade without rexctl. "+
		"Or restore your latest backup with sudo rexctl restore",
		e.DataDir, e.Major, e.Have, e.Major)
}

func joinMajors(installed []Install) string {
	var s []string
	for _, in := range installed {
		s = append(s, strconv.Itoa(in.Major))
	}
	switch len(s) {
	case 0:
		return ""
	case 1:
		return s[0]
	}
	return strings.Join(s[:len(s)-1], ", ") + " and " + s[len(s)-1]
}

func (c Choice) Hint() string {
	if c.Newer == 0 {
		return ""
	}
	return fmt.Sprintf("the database is PostgreSQL %d and PostgreSQL %d is installed. Run sudo rexctl postgres upgrade, or fluxerd postgres-upgrade without rexctl, to move to %d", c.Major, c.Newer, c.Newer)
}
