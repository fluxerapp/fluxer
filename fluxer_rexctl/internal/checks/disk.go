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
	firstStartBytes = 256 << 20
	diskReserve     = backup.DiskReserve
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
		return e.reserveDisk(spaces([]Space{{Path: e.imageStore(), Images: true}, {Path: e.Dir}}))
	}
	spaces := e.InstallNeed(ctx)
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
		img := spaces[0]
		growth := e.growthCap(img.Path)
		next := e.nextReleaseNeed()
		has := fmt.Sprintf("%s has %s of free disk space and this install needs about %s.", e.diskRole(img.Path), size(img.Free), size(img.Need))
		switch {
		case img.Free < img.Need+growth:
			r = warn(fmt.Sprintf("%s Fluxer's logs, database write-ahead log, upload spool and message queue can then use up to %s more as it runs, plus the space for messages and uploads.", has, size(growth)),
				fmt.Sprintf("Free up about %s more on the disk that holds %s.", size(img.Need+growth-img.Free), img.Path))
		case img.Free < img.Need+next:
			r = warn(fmt.Sprintf("%s The next upgrade needs about %s more to download the new release.", has, size(next)),
				fmt.Sprintf("Free up about %s more on the disk that holds %s before you upgrade.", size(img.Need+next-img.Free), img.Path))
		default:
			r = pass(has)
		}
	}
	e.noteVMDisk(&r)
	return r
}

func (e *Env) installedDisk(ctx context.Context) Result {
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
	switch path {
	case e.Dir:
		return fmt.Sprintf("%s, the instance folder,", path)
	case e.imageStore():
		return fmt.Sprintf("%s, where the container engine keeps Fluxer's images and data,", path)
	}
	return fmt.Sprintf("%s, the backup folder,", path)
}

func (e *Env) installDiskFix(path string) string {
	if path != e.Dir {
		return fmt.Sprintf("Free up space on the disk that holds %s, or move the container engine's data root to a bigger disk, then run rexctl doctor.", path)
	}
	return fmt.Sprintf("Free up space on the disk that holds %s, or pass --dir with a folder on a bigger disk.", path)
}

func (e *Env) imageStore() string { return engine.ImageStore(e.Dir, e.Engine) }

type Space struct {
	Path   string
	Images bool
	Need   uint64
	Free   uint64
}

func (s Space) Short() bool { return s.Free < s.Need }

func (e *Env) UpgradeSpace(pull, backup uint64) []Space {
	wants := []Space{{Path: e.imageStore(), Images: true, Need: pull + e.startAllowance()}, {Path: e.Dir}}
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
	return firstStartBytes + stack.WALCap(stack.Embedded(), e.effective())
}

func (e *Env) installRefs() (stack.ImageSet, []string) {
	images, _ := stack.EmbeddedImages()
	return images, RunningRefs(stack.Embedded(), images.Images, e.runningSet())
}

func (e *Env) InstallNeed(ctx context.Context) []Space {
	images, refs := e.installRefs()
	need := images.Need(e.platform(), refs, e.localDiffIDs(ctx))
	return spaces([]Space{{Path: e.imageStore(), Images: true, Need: need + firstStartBytes}, {Path: e.Dir}})
}

func (e *Env) nextReleaseNeed() uint64 {
	images, refs := e.installRefs()
	return images.Need(e.platform(), refs, nil)
}

func (e *Env) NextUpgradeNeed(ctx context.Context) []Space {
	var pull uint64
	if st := e.state(); st != nil && e.Engine != nil {
		images, _ := stack.EmbeddedImages()
		pull = ReleaseSize(ctx, e.Engine, images, slices.Sorted(maps.Values(st.Images.Refs)))
	}
	var saved uint64
	if e.Instance != nil && e.Project != nil && !e.Instance.State.IsVoice() {
		saved, _ = backup.Estimate(ctx, e.Instance, e.Project, backup.ScopeFull)
	}
	return e.UpgradeSpace(pull, saved)
}

func ReleaseSize(ctx context.Context, en *engine.Engine, set stack.ImageSet, refs []string) uint64 {
	var known []string
	var need uint64
	for _, ref := range slices.Compact(slices.Sorted(slices.Values(refs))) {
		if _, ok := set.LayersFor(en.Platform, ref); ok {
			known = append(known, ref)
			continue
		}
		if n, found, err := en.ImageSize(ctx, ref); err == nil && found {
			need += n
			continue
		}
		need += stack.ImageGuess
	}
	return need + set.Need(en.Platform, known, nil)
}

func PullSpace(ctx context.Context, en *engine.Engine, dir string, refs []string, sizes map[string]uint64) (Space, bool) {
	images, _ := stack.EmbeddedImages()
	need := en.PullNeed(ctx, images, refs, sizes)
	if need == 0 {
		return Space{}, false
	}
	out := spaces([]Space{{Path: engine.ImageStore(dir, en), Images: true, Need: need}})
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

func (e *Env) runningSet() Running {
	return Running{
		Voice:         stack.VoiceBundled(e.value(stack.VoiceBundledKey)),
		ExternalStore: strings.TrimSpace(e.chosen(settings.StorageEndpoint)) != "",
		Chosen:        e.chosen,
	}
}

func (e *Env) platform() string {
	if e.Engine == nil {
		return ""
	}
	return e.Engine.Platform
}

func (e *Env) localDiffIDs(ctx context.Context) map[string]bool {
	if e.Engine == nil {
		return nil
	}
	ids, err := e.Engine.LocalDiffIDs(ctx)
	if err != nil {
		return nil
	}
	return ids
}

func (e *Env) growthCap(store string) uint64 {
	if e.installed() {
		return stack.GrowthCap(stack.Embedded(), e.effective())
	}
	profile := e.chosen(settings.DiskProfile)
	if _, set := e.Settings[settings.DiskProfile]; !set {
		if total, err := host.DiskSize(store); err == nil {
			profile = stack.DiskProfileFor(total)
		}
	}
	return stack.GrowthCap(stack.Embedded(), stack.DiskSizing(profile, nil))
}

type Running struct {
	Voice         bool
	ExternalStore bool
	Chosen        func(string) string
}

func RunningRefs(set *stack.Set, fluxer map[string]string, run Running) []string {
	compose, err := stack.ReadFile(set, stack.ComposeFile)
	if err != nil {
		return nil
	}
	chosen := run.Chosen
	if chosen == nil {
		chosen = func(string) string { return "" }
	}
	external := run.ExternalStore
	var out []string
	for svc, value := range stack.ComposeImages(compose) {
		switch {
		case svc == "livekit" && !run.Voice:
			continue
		case (svc == "seaweedfs" || svc == "seaweedfs-init") && external:
			continue
		}
		ref := imageFor(svc, value, fluxer, chosen)
		if !slices.Contains(out, ref) {
			out = append(out, ref)
		}
	}
	slices.Sort(out)
	return out
}

func imageFor(svc, value string, fluxer map[string]string, chosen func(string) string) string {
	if strings.HasPrefix(value, "${FLUXER_REGISTRY") {
		if ref := fluxer[svc]; ref != "" {
			return ref
		}
		return svc
	}
	def, ok := stack.ImageDefault(value)
	if !ok {
		return value
	}
	name := strings.TrimPrefix(strings.SplitN(value, ":-", 2)[0], "${")
	if v := strings.TrimSpace(chosen(name)); v != "" {
		return v
	}
	for key, v := range settings.ImageVars {
		if v == name {
			if ref := strings.TrimSpace(chosen(settings.ImagesPrefix + key)); ref != "" {
				return ref
			}
		}
	}
	return def
}
