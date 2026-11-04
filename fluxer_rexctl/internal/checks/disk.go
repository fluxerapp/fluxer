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
	imageGuess      = 1 << 30
	pullScratch     = 3
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
		r = pass(fmt.Sprintf("%s has %s of free disk space, and this install needs about %s.", e.diskRole(img.Path), size(img.Free), size(img.Need)))
		if growth := e.growthCap(img.Path); img.Free < img.Need+growth {
			r = warn(fmt.Sprintf("%s has %s of free disk space. Fluxer fits, but its logs and database can grow by up to %s as it runs, before counting messages and uploads.", e.diskRole(img.Path), size(img.Free), size(growth)),
				fmt.Sprintf("Free up space on the disk that holds %s.", img.Path))
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
		r = fail(fmt.Sprintf("%s has only %s of free disk space left, and Fluxer needs at least %s free to keep its database writing.", e.diskRole(lowest.Path), size(lowest.Free), size(diskReserve)),
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

func (e *Env) Space(pull, backup uint64) []Space {
	wants := []Space{{Path: e.imageStore(), Images: true, Need: pull}, {Path: e.Dir}}
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

func (e *Env) InstallNeed(ctx context.Context) []Space {
	images, _ := stack.EmbeddedImages()
	refs := RunningRefs(stack.Embedded(), images.Images, e.runningSet())
	need := ImageNeed(images, e.platform(), refs, e.localDiffIDs(ctx), e.keepsBlobs())
	return spaces([]Space{{Path: e.imageStore(), Images: true, Need: need + firstStartBytes}, {Path: e.Dir}})
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
	return e.Space(pull, saved)
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
		need += imageGuess
	}
	return need + ImageNeed(set, en.Platform, known, nil, en.KeepsBlobs)
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

func (e *Env) keepsBlobs() bool { return e.Engine != nil && e.Engine.KeepsBlobs }

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

func ImageNeed(set stack.ImageSet, platform string, refs []string, local map[string]bool, keepsBlobs bool) uint64 {
	var need uint64
	var downloads []uint64
	seen := map[string]bool{}
	guessed := map[string]bool{}
	for _, ref := range refs {
		layers, ok := set.LayersFor(platform, ref)
		if !ok {
			if !guessed[ref] {
				guessed[ref] = true
				need += imageGuess
			}
			continue
		}
		for _, l := range layers {
			if local[l.DiffID] || seen[l.DiffID] {
				continue
			}
			seen[l.DiffID] = true
			need += l.Unpacked
			if keepsBlobs {
				need += l.Compressed
			} else {
				downloads = append(downloads, l.Compressed)
			}
		}
	}
	slices.Sort(downloads)
	for i := 0; i < pullScratch && i < len(downloads); i++ {
		need += downloads[len(downloads)-1-i]
	}
	return need
}

func PullNeed(ctx context.Context, e *engine.Engine, set stack.ImageSet, refs []string) uint64 {
	var local map[string]bool
	var missing []string
	if ids, err := e.LocalDiffIDs(ctx); err == nil {
		local = ids
	}
	for _, ref := range refs {
		if _, found, err := e.LocalImage(ctx, ref); err == nil && found {
			continue
		}
		missing = append(missing, ref)
	}
	return ImageNeed(set, e.Platform, missing, local, e.KeepsBlobs)
}
