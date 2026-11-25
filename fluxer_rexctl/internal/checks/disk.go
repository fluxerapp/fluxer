// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"
	"maps"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	firstStartBytes    = 256 << 20
	diskReserve        = backup.DiskReserve
	nativeReleaseBytes = 3_000_000_000
)

const vmDiskNote = "The container engine keeps its data in a virtual machine disk, and rexctl can only check this machine's disk."

func init() {
	Register(Check{ID: "disk", Phases: Preinstall | Installed | Voice, Run: runDisk})
}

func runDisk(ctx context.Context, e *Env) Result {
	switch {
	case e.installed():
		return e.installedDisk(ctx)
	case e.phase&Voice != 0:
		return e.reserveDisk(spaces(e.wants(0, 0)))
	}
	if e.Native {
		return e.nativeInstallDisk()
	}
	spaces := e.InstallNeed()
	if len(spaces) == 0 {
		return skip("Couldn't read how much free disk space this machine has.")
	}
	var r Result
	for _, sp := range spaces {
		if sp.Short() {
			r = fail(fmt.Sprintf("%s has %s of free disk space and this install needs about %s there.", e.diskRole(sp.Path), size(sp.Free), size(sp.Need)),
				e.installDiskFix(sp.Path))
			break
		}
	}
	if r.Status == "" {
		stores := e.stores()
		data, dataFound := spaceOn(spaces, stores.Data)
		img, imgFound := spaceOn(spaces, stores.Images)
		growth := e.growthCap(stores.Data)
		next := uint64(nativeReleaseBytes)
		has := func(sp Space) string {
			return fmt.Sprintf("%s has %s of free disk space and this install needs about %s.", e.diskRole(sp.Path), size(sp.Free), size(sp.Need))
		}
		switch {
		case dataFound && data.Free < data.Need+growth:
			r = warn(fmt.Sprintf("%s Fluxer's logs, database write-ahead log, upload spool and message queue can then use up to %s more as it runs, plus the space for messages and uploads.", has(data), size(growth)),
				fmt.Sprintf("Free up about %s more on the disk that holds %s.", size(data.Need+growth-data.Free), data.Path))
		case imgFound && img.Free < img.Need+next:
			r = warn(fmt.Sprintf("%s The next upgrade needs about %s more to download the new release.", has(img), size(next)),
				fmt.Sprintf("Free up about %s more on the disk that holds %s before you upgrade.", size(img.Need+next-img.Free), img.Path))
		default:
			r = pass(has(spaces[0]))
		}
	}
	e.noteVMDisk(&r)
	return r
}

func (e *Env) nativeInstallDisk() Result {
	sp := spaces([]Space{{Path: e.Dir, Need: nativeReleaseBytes + firstStartBytes}})
	if len(sp) == 0 {
		return skip("Couldn't read how much free disk space this machine has.")
	}
	has := fmt.Sprintf("%s has %s of free disk space and this install needs about %s there.", e.diskRole(sp[0].Path), size(sp[0].Free), size(sp[0].Need))
	if sp[0].Short() {
		return fail(has, e.installDiskFix(sp[0].Path))
	}
	return pass(has)
}

func (e *Env) installedDisk(ctx context.Context) Result {
	if e.Container {
		return e.reserveDisk(spaces([]Space{{Path: e.Dir}}))
	}
	spaces := e.NextUpgradeNeed(ctx)
	r := e.reserveDisk(spaces)
	if r.Status != Pass {
		return r
	}
	for _, sp := range spaces {
		if sp.Short() {
			r = warn(fmt.Sprintf("%s has %s of free disk space left and the next upgrade needs about %s.", e.diskRole(sp.Path), size(sp.Free), size(sp.Need)),
				fmt.Sprintf("Free up space on the disk that holds %s before you upgrade.", sp.Path))
			e.noteVMDisk(&r)
			return r
		}
	}
	return r
}

func (e *Env) reserveDisk(spaces []Space) Result {
	if len(spaces) == 0 {
		return skip("Couldn't read how much free disk space this machine has.")
	}
	lowest := spaces[0]
	for _, sp := range spaces[1:] {
		if sp.Free < lowest.Free {
			lowest = sp
		}
	}
	r := pass(fmt.Sprintf("%s has %s of free disk space.", e.diskRole(lowest.Path), size(lowest.Free)))
	if lowest.Free < diskReserve {
		r = fail(fmt.Sprintf("%s has only %s of free disk space left. Fluxer needs at least %s free so the database can keep saving changes.", e.diskRole(lowest.Path), size(lowest.Free), size(diskReserve)),
			fmt.Sprintf("Free up space on the disk that holds %s, then run rexctl doctor.", lowest.Path))
	}
	e.noteVMDisk(&r)
	return r
}

func (e *Env) noteVMDisk(r *Result) {
	if e.Engine != nil && e.Engine.Desktop != "" {
		r.Details = append(r.Details, vmDiskNote)
	}
}

func size(n uint64) string { return ui.Bytes(int64(n)) }

func (e *Env) diskRole(path string) string {
	if path == e.Dir {
		return fmt.Sprintf("%s, the instance folder,", path)
	}
	stores := e.stores()
	keeps := "keeps"
	if e.Engine == nil {
		keeps = "will keep"
	}
	switch {
	case path == stores.Images && sameDisk(stores.Images, stores.Data):
		return fmt.Sprintf("%s, where the container engine %s Fluxer's images and data,", path, keeps)
	case path == stores.Images:
		return fmt.Sprintf("%s, where the container engine %s Fluxer's images,", path, keeps)
	case path == stores.Data:
		return fmt.Sprintf("%s, where the container engine %s Fluxer's data,", path, keeps)
	}
	return fmt.Sprintf("%s, the backup folder,", path)
}

func (e *Env) installDiskFix(path string) string {
	if path == e.Dir {
		return fmt.Sprintf("Free up space on the disk that holds %s, or pass --dir with a folder on a bigger disk.", path)
	}
	if stores := e.stores(); path == stores.Images && stores.Images != stores.Data && !strings.HasPrefix(stores.Images, stores.Data+"/") {
		return fmt.Sprintf("Free up space on the disk that holds %s, or set \"root\" in %s to a folder on a bigger disk, then run rexctl install again.", path, engine.ContainerdConfig)
	}
	return fmt.Sprintf("Free up space on the disk that holds %s, or move the container engine's data root to a bigger disk, then run rexctl install again.", path)
}

func (e *Env) stores() engine.Roots {
	if e.Engine != nil {
		return engine.Stores(e.Dir, e.Engine, "")
	}
	if e.planned == nil {
		planned := engine.Stores(e.Dir, nil, e.Prefer)
		e.planned = &planned
	}
	return *e.planned
}

func (e *Env) wants(pull, data uint64) []Space {
	if e.Native {
		return []Space{{Path: e.Dir, Need: pull + data}}
	}
	stores := e.stores()
	var out []Space
	if stores.Images != "" {
		out = append(out, Space{Path: stores.Images, Images: true, Need: pull})
	}
	return append(out, Space{Path: stores.Data, Need: data}, Space{Path: e.Dir})
}

func spaceOn(list []Space, path string) (Space, bool) {
	if path == "" {
		return Space{}, false
	}
	for _, sp := range list {
		if sp.Path == path || sameDisk(sp.Path, path) {
			return sp, true
		}
	}
	return Space{}, false
}

func sameDisk(a, b string) bool {
	if a == b {
		return true
	}
	x, errX := host.DiskID(a)
	y, errY := host.DiskID(b)
	return errX == nil && errY == nil && x == y
}

type Space struct {
	Path   string
	Images bool
	Need   uint64
	Free   uint64
}

func (s Space) Short() bool { return s.Free < s.Need }

func (e *Env) UpgradeSpace(pull, backup uint64) []Space {
	wants := e.wants(pull, e.startAllowance())
	if e.Instance != nil {
		wants = append(wants, Space{Path: e.Instance.BackupDir(), Need: backup})
	}
	return spaces(wants)
}

func spaces(wants []Space) []Space {
	var out []Space
	disks := map[string]int{}
	for _, w := range wants {
		id, err := host.DiskID(w.Path)
		if err != nil {
			id = w.Path
		}
		if i, ok := disks[id]; ok {
			out[i].Need += w.Need
			continue
		}
		free, err := host.FreeDisk(w.Path)
		if err != nil {
			continue
		}
		disks[id] = len(out)
		w.Free = free
		w.Need += diskReserve
		out = append(out, w)
	}
	return out
}

func (e *Env) startAllowance() uint64 {
	if !e.installed() {
		return firstStartBytes
	}
	return firstStartBytes + stack.WALCap(e.releaseSet(), e.effective())
}

func (e *Env) InstallNeed() []Space {
	return spaces(e.wants(nativeReleaseBytes, firstStartBytes))
}

func (e *Env) NextUpgradeNeed(ctx context.Context) []Space {
	if e.Native {
		return e.UpgradeSpace(nativeReleaseBytes, 0)
	}
	var pull uint64
	if st := e.state(); st != nil && e.Engine != nil {
		pull = ReleaseSize(ctx, e.Engine, slices.Sorted(maps.Values(st.Images.Refs)))
	}
	var saved uint64
	if e.Instance != nil && e.Project != nil && !e.Instance.State.IsVoice() {
		saved, _ = backup.Estimate(ctx, e.Instance, e.Project, backup.ScopeFull)
	}
	return e.UpgradeSpace(pull, saved)
}

func ReleaseSize(ctx context.Context, en *engine.Engine, refs []string) uint64 {
	var need uint64
	for _, ref := range slices.Compact(slices.Sorted(slices.Values(refs))) {
		if n, found, err := en.ImageSize(ctx, ref); err == nil && found {
			need += n
			continue
		}
		need += engine.ImageGuess
	}
	return need
}

func PullSpace(ctx context.Context, en *engine.Engine, dir string, refs []string, sizes map[string]uint64) (Space, bool) {
	need := en.PullNeed(ctx, refs, sizes)
	if need == 0 {
		return Space{}, false
	}
	out := spaces([]Space{{Path: engine.Stores(dir, en, "").Images, Images: true, Need: need}})
	if len(out) == 0 {
		return Space{}, false
	}
	return out[0], true
}

func (e *Env) chosen(key string) string {
	if v, ok := e.Settings[key]; ok {
		return v
	}
	return e.setting(key)
}

func (e *Env) growthCap(store string) uint64 {
	if e.installed() {
		return stack.GrowthCap(e.releaseSet(), e.effective())
	}
	profile := e.chosen(settings.DiskProfile)
	if _, set := e.Settings[settings.DiskProfile]; !set {
		if total, err := host.DiskSize(store); err == nil {
			profile = stack.DiskProfileFor(total)
		}
	}
	return stack.GrowthCap(e.releaseSet(), stack.DiskSizing(profile, nil))
}

func (e *Env) releaseSet() *stack.Set {
	if e.state() != nil {
		if set, err := e.Instance.Set(); err == nil {
			return set
		}
	}
	return settings.NativeSet(stack.EmbeddedRelease())
}

type Running struct {
	Voice         bool
	ExternalStore bool
	Chosen        func(string) string
}
