// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	goruntime "runtime"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	postgresVolume = "postgres-data"
	dataStampFile  = "VERSION"
)

type nativeUpgrades struct{ r *nativeRT }

func (u nativeUpgrades) Current(ctx context.Context) (string, error) {
	return u.r.inst.State.Release, nil
}

func (u nativeUpgrades) Check(ctx context.Context, to *release.Release) error {
	if _, err := to.DistFor(goruntime.GOARCH); err != nil {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: err.Error(), Fix: "Try again after the next Fluxer release.", Err: ErrUnsupported}
	}
	return nil
}

var postgresPackage = regexp.MustCompile(`^postgresql-(\d+)$`)

func (u nativeUpgrades) postgresMajor(version string) (int, bool) {
	packages, err := u.r.n.Packages(version)
	if err != nil {
		return 0, false
	}
	for _, p := range packages {
		if m := postgresPackage.FindStringSubmatch(p); m != nil {
			n, err := strconv.Atoi(m[1])
			return n, err == nil
		}
	}
	return 0, false
}

func (u nativeUpgrades) clusterMajor() (int, bool) {
	dir, _ := u.r.n.DataDir(postgresVolume)
	data, err := os.ReadFile(filepath.Join(dir, "PG_VERSION"))
	if err != nil {
		return 0, false
	}
	n, err := strconv.Atoi(strings.TrimSpace(string(data)))
	return n, err == nil
}

func (u nativeUpgrades) checkPostgres(version string) error {
	have, ok := u.clusterMajor()
	if !ok {
		return nil
	}
	want, ok := u.postgresMajor(version)
	if !ok || want == have {
		return nil
	}
	cur := u.r.inst.State.Release
	if release.Compare(version, u.r.inst.State.Release) < 0 {
		return ui.Refused(fmt.Sprintf("Fluxer %s runs Postgres %d, and your database is on Postgres %d now.", version, want, have), u.r.inst.State.RestoreDatabaseFix())
	}
	return ui.Refused(fmt.Sprintf("Fluxer %s moves your database from Postgres %d to %d, and rexctl can't do that yet.", version, have, want), "Nothing changed. Stay on Fluxer "+cur+" until rexctl can move the database for you.")
}

func (u nativeUpgrades) CheckBack(ctx context.Context, to string) error {
	if err := u.checkPostgres(to); err != nil {
		return err
	}
	if u.DataNewer(to) {
		return ui.Refused(fmt.Sprintf("Fluxer %s has changed your data, so Fluxer %s can't start on it.", u.r.inst.State.Release, to), u.r.inst.State.RestoreDatabaseFix())
	}
	return nil
}

func (u nativeUpgrades) DataNewer(version string) bool {
	data, err := os.ReadFile(filepath.Join(u.r.n.Root(), dataStampFile))
	if err != nil {
		return false
	}
	stamped := strings.TrimSpace(string(data))
	return release.Valid(stamped) && release.Valid(version) && release.Compare(stamped, version) > 0
}

func (u nativeUpgrades) Sizes(ctx context.Context, to *release.Release, backup string) (download, saved int64) {
	download, saved = -1, -1
	if u.r.n.Has(to.Version) {
		download = 0
	} else if n := treeSize(u.r.n.ReleaseDir(u.r.inst.State.Release)); n > 0 {
		download = int64(n)
	}
	if backup == settings.BackupOff {
		saved = 0
	} else {
		saved = int64(treeSize(u.r.n.Root()))
	}
	return download, saved
}

func (u nativeUpgrades) CheckSpace(ctx context.Context, to *release.Release, download, saved int64) error {
	type need struct {
		path  string
		bytes uint64
	}
	var needs []need
	add := func(path string, n int64) {
		if n <= 0 {
			return
		}
		id, err := host.DiskID(path)
		for i := range needs {
			if other, oerr := host.DiskID(needs[i].path); err == nil && oerr == nil && other == id {
				needs[i].bytes += uint64(n)
				return
			}
		}
		needs = append(needs, need{path: path, bytes: uint64(n)})
	}
	add(u.r.n.ReleasesDir(), 2*download)
	add(nearestDir(u.r.inst.BackupDir()), saved)
	for _, n := range needs {
		free, err := host.FreeDisk(n.path)
		if err != nil || free >= n.bytes {
			continue
		}
		fix := "Free up space there, then run rexctl upgrade again."
		if saved > 0 {
			fix = "Free up space there, then run rexctl upgrade again. To upgrade with no backup, run rexctl upgrade --no-backup. If the upgrade then goes wrong, there is no backup from before it to restore."
		}
		return ui.Refused(
			fmt.Sprintf("Fluxer %s needs about %s of free disk space in %s, and %s is free.", to.Version, ui.Bytes(int64(n.bytes)), n.path, ui.Bytes(int64(free))),
			fix,
		)
	}
	return nil
}

func nearestDir(path string) string {
	for {
		if _, err := os.Stat(path); err == nil {
			return path
		}
		parent := filepath.Dir(path)
		if parent == path {
			return path
		}
		path = parent
	}
}

func treeSize(dir string) uint64 {
	var total uint64
	_ = filepath.WalkDir(dir, func(_ string, d fs.DirEntry, err error) error {
		if err != nil || d == nil {
			return nil
		}
		if d.Type().IsRegular() {
			if info, err := d.Info(); err == nil {
				total += uint64(info.Size())
			}
		}
		return nil
	})
	return total
}

func (u nativeUpgrades) Prepare(ctx context.Context, s *ui.Step, to *release.Release) error {
	if err := u.r.root(); err != nil {
		return err
	}
	short := to.Version
	if _, err := u.r.n.Fetch(ctx, to, progressOf(s)); err != nil {
		return NativeError(err, "Couldn't download Fluxer "+short+".")
	}
	if _, err := u.r.n.WriteSettings(to.Version); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't write the settings of Fluxer %s to %s.", short, u.r.inst.Dir), "Check that you can write there, then run rexctl upgrade again.")
	}
	if err := u.checkPostgres(to.Version); err != nil {
		return err
	}
	var apt native.AptReporter
	if s != nil {
		apt = s
	}
	if err := u.r.n.EnsurePackages(ctx, to.Version, apt); err != nil {
		return NativeError(err, "Couldn't install the system packages Fluxer "+short+" needs.")
	}
	return NativeError(native.EnsureUser(ctx), "Couldn't create the account "+native.ServiceUser+".")
}

func (u nativeUpgrades) Switch(ctx context.Context, s *ui.Step, to *release.Release) error {
	if !u.r.n.Has(to.Version) {
		return ui.Failed(fmt.Sprintf("Fluxer %s isn't unpacked in %s.", to.Version, u.r.n.ReleasesDir()), "Run rexctl upgrade again.")
	}
	return NativeError(u.r.n.Flip(to.Version), "Couldn't switch to Fluxer "+to.Version+".")
}

func (u nativeUpgrades) StartVerify(ctx context.Context, s *ui.Step) error {
	return u.r.Restart(ctx, s, nil)
}

func (u nativeUpgrades) GoBack(ctx context.Context, s *ui.Step, from string) error {
	if u.r.n.Running() == from {
		return nil
	}
	if !u.r.n.Has(from) {
		return ui.Failed(fmt.Sprintf("Fluxer %s isn't on this machine anymore.", from), "Run rexctl upgrade to start the newer release again instead.")
	}
	return NativeError(u.r.n.Flip(from), "Couldn't go back to Fluxer "+from+".")
}

func (u nativeUpgrades) Prune(ctx context.Context, keep []string) error {
	if err := release.PruneOlder(ctx, u.r.inst, nil); err != nil {
		return err
	}
	entries, err := os.ReadDir(u.r.n.ReleasesDir())
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	keep = append(slices.Clone(keep), u.r.n.Running(), u.r.inst.State.Release, u.r.inst.State.PreviousRelease)
	for _, e := range entries {
		name := e.Name()
		if strings.HasPrefix(name, ".") || slices.Contains(keep, name) {
			continue
		}
		if err := os.RemoveAll(filepath.Join(u.r.n.ReleasesDir(), name)); err != nil {
			return err
		}
	}
	return nil
}

func (u nativeUpgrades) Has(version string) bool { return u.r.n.Has(version) }

func (u nativeUpgrades) Settle(target string) (bool, error) {
	if target == "" || u.r.n.Running() == target || !u.r.n.Has(target) {
		return false, nil
	}
	return true, u.r.n.Flip(target)
}

func (u nativeUpgrades) Secrets() (map[string]string, error) { return u.r.n.ReadSecrets() }
