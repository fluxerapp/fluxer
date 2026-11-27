// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	imageKey     = "fluxer"
	ImageRoot    = "/opt/fluxer/current"
	imageCatalog = ImageRoot + "/share/settings.json"
)

type containerUpgrades struct{ r *containerRT }

func (u containerUpgrades) Current(ctx context.Context) (string, error) {
	return u.r.inst.State.Release, nil
}

func (u containerUpgrades) Check(ctx context.Context, to *release.Release) error {
	if !release.Valid(to.Version) {
		return ui.Refused(fmt.Sprintf("Fluxer %s has no image to run in a container.", to.Version), "Pass a released version with --to.")
	}
	return nil
}

func (u containerUpgrades) Has(version string) bool {
	e, err := u.r.engine(context.Background())
	if err != nil {
		return false
	}
	_, ok, err := e.LocalImage(context.Background(), ImageRef(version))
	return err == nil && ok
}

type imageSource interface {
	LocalImage(ctx context.Context, ref string) (string, bool, error)
	PullImage(ctx context.Context, ref string, onEvent func(engine.Event)) error
	RepoDigestsOf(ctx context.Context, ref string) []string
}

func FetchImage(ctx context.Context, e imageSource, version string, onPull func(engine.Event)) error {
	ref := ImageRef(version)
	_, here, err := e.LocalImage(ctx, ref)
	if err != nil {
		return err
	}
	if !here {
		if err := e.PullImage(ctx, ref, onPull); err != nil {
			return err
		}
	}
	return checkDigest(ctx, e, version)
}

func checkDigest(ctx context.Context, e imageSource, version string) error {
	ref := ImageRef(version)
	if want := pinnedDigest(version); want != "" && !slices.Contains(e.RepoDigestsOf(ctx, ref), imageRepo()+"@"+want) {
		return fmt.Errorf("%w: %s should be %s", ErrImageDigest, ref, want)
	}
	return nil
}

func (r *containerRT) pull(ctx context.Context, s *ui.Step, version string) error {
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	ref := ImageRef(version)
	layers := map[string][2]int64{}
	err = FetchImage(ctx, e, version, func(ev engine.Event) {
		if s == nil || ev.Total <= 0 {
			return
		}
		layers[ev.ID] = [2]int64{ev.Current, ev.Total}
		var cur, total int64
		for _, l := range layers {
			cur, total = cur+l[0], total+l[1]
		}
		s.Progress(cur, total)
	})
	if err != nil {
		return err
	}
	st := r.inst.State
	if st.Images.Refs == nil {
		st.Images.Refs = map[string]string{}
	}
	st.Images.Refs[imageKey] = ref
	if digest := e.RepoDigestOf(ctx, ref); digest != "" {
		st.Images.Refs[imageKey] = digest
	}
	return nil
}

func (r *containerRT) fetch(ctx context.Context, s *ui.Step, version string) error {
	if err := r.pull(ctx, s, version); err != nil {
		return ContainerError(err, "Couldn't download Fluxer "+version+".")
	}
	set, err := r.imageSet(ctx, version)
	if err != nil {
		return ContainerError(err, "Couldn't read the settings of Fluxer "+version+".")
	}
	if err := release.WriteRelease(r.inst, set); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't write the settings of Fluxer %s to %s.", version, r.inst.Dir), "Check that you can write there, then try again.")
	}
	return saveState(r.inst)
}

func PullImage(ctx context.Context, r Runtime, s *ui.Step, version string) error {
	c, ok := r.(*containerRT)
	if !ok {
		return ErrUnsupported
	}
	return c.fetch(ctx, s, version)
}

func PrepareContainer(ctx context.Context, r Runtime) error {
	c, ok := r.(*containerRT)
	if !ok {
		return ErrUnsupported
	}
	return c.Prepare(ctx)
}

func (u containerUpgrades) Prepare(ctx context.Context, s *ui.Step, to *release.Release) error {
	return u.r.fetch(ctx, s, to.Version)
}

func (r *containerRT) imageSet(ctx context.Context, version string) (*stack.Set, error) {
	e, err := r.engine(ctx)
	if err != nil {
		return nil, err
	}
	name := r.name() + "-catalog"
	_ = e.RemoveSingle(ctx, name)
	if err := e.CreateSingle(ctx, engine.SingleSpec{Name: name, Image: ImageRef(version), Labels: map[string]string{LabelManagedBy: ManagedByRexctl}}); err != nil {
		return nil, err
	}
	defer func() { _ = e.RemoveSingle(context.WithoutCancel(ctx), name) }()
	data, err := e.ReadSingleFile(ctx, name, imageCatalog)
	if errors.Is(err, engine.ErrNoFile) {
		return settings.NativeSet(version), nil
	}
	if err != nil {
		return nil, err
	}
	catalog, err := stack.ParseCatalog(data)
	if err != nil {
		return nil, err
	}
	set := stack.CatalogSet(version, catalog)
	return set, nil
}

func (r *containerRT) setFor(version string) (*stack.Set, error) {
	if version == r.inst.State.Release {
		return deploy.ReleaseSet(r.inst)
	}
	set, err := release.LoadSet(r.inst, version)
	if errors.Is(err, release.ErrNoSnapshot) {
		return settings.NativeSet(version), nil
	}
	return set, err
}

func (u containerUpgrades) switchTo(ctx context.Context, version string) error {
	r := u.r
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	set, err := r.setFor(version)
	if err != nil {
		return err
	}
	spec, err := r.spec(set, version)
	if err != nil {
		return err
	}
	name, prev := spec.Name, PreviousName(r.inst)
	cur, ok, err := e.Inspect(ctx, name)
	if err != nil {
		return err
	}
	old, hasOld, err := e.Inspect(ctx, prev)
	if err != nil {
		return err
	}
	switch {
	case ok && cur.Labels[LabelVersion] == version:
		if cur.Labels[labelSpec] == spec.Labels[labelSpec] {
			_, err := r.push(ctx, e, set)
			return err
		}
		return r.replace(ctx, e, spec, set)
	case hasOld && old.Labels[LabelVersion] == version:
		if ok {
			if err := e.StopSingle(ctx, name, ContainerStop); err != nil {
				return err
			}
			if err := e.RemoveSingle(ctx, name); err != nil {
				return err
			}
		}
		if err := e.RenameSingle(ctx, prev, name); err != nil {
			return err
		}
		if old.Labels[labelSpec] != spec.Labels[labelSpec] {
			return r.replace(ctx, e, spec, set)
		}
		if err := e.SetRestart(ctx, name, spec.Restart); err != nil {
			return err
		}
		_, err := r.push(ctx, e, set)
		return err
	case ok:
		if err := e.StopSingle(ctx, name, ContainerStop); err != nil {
			return err
		}
		if hasOld {
			if err := e.RemoveSingle(ctx, prev); err != nil {
				return err
			}
		}
		if err := e.RenameSingle(ctx, name, prev); err != nil {
			return err
		}
		if err := e.SetRestart(ctx, prev, restartNever); err != nil {
			return err
		}
	}
	return r.create(ctx, e, spec, set)
}

func (u containerUpgrades) Switch(ctx context.Context, s *ui.Step, to *release.Release) error {
	return ContainerError(u.switchTo(ctx, to.Version), "Couldn't switch to Fluxer "+to.Version+".")
}

func (u containerUpgrades) StartVerify(ctx context.Context, s *ui.Step) error {
	return u.r.Start(ctx, s, StartOptions{})
}

func (u containerUpgrades) GoBack(ctx context.Context, s *ui.Step, from string) error {
	return ContainerError(u.switchTo(ctx, from), "Couldn't go back to Fluxer "+from+".")
}

func (u containerUpgrades) Prune(ctx context.Context, keep []string) error {
	r := u.r
	if err := release.PruneOlder(ctx, r.inst, nil); err != nil {
		return err
	}
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	if err := e.RemoveSingle(ctx, PreviousName(r.inst)); err != nil {
		return err
	}
	tags, err := e.ImageTags(ctx, imageRepo())
	if err != nil {
		return err
	}
	st := r.inst.State
	keep = append(slices.Clone(keep), st.Release, st.PreviousRelease)
	for _, tag := range tags {
		if !release.Valid(tag) || slices.Contains(keep, tag) {
			continue
		}
		if err := e.RemoveImages(ctx, []string{ImageRef(tag)}); err != nil && !engine.InUse(err) {
			return err
		}
	}
	return nil
}

func (u containerUpgrades) stamp(ctx context.Context) string {
	e, err := u.r.engine(ctx)
	if err != nil {
		return ""
	}
	for _, name := range []string{u.r.name(), PreviousName(u.r.inst)} {
		if data, err := e.ReadSingleFile(ctx, name, ContainerData+"/"+containerStampRel); err == nil {
			return strings.TrimSpace(string(data))
		}
	}
	return ""
}

func (u containerUpgrades) DataNewer(version string) bool {
	stamped := u.stamp(context.Background())
	return release.Valid(stamped) && release.Valid(version) && release.Compare(stamped, version) > 0
}

func (u containerUpgrades) CheckBack(ctx context.Context, to string) error {
	if u.DataNewer(to) {
		return ui.Refused(fmt.Sprintf("Fluxer %s has changed your data, so Fluxer %s can't start on it.", u.r.inst.State.Release, to), u.r.inst.State.RestoreDatabaseFix())
	}
	return nil
}

func (u containerUpgrades) Secrets() (map[string]string, error) {
	return u.r.ReadSecrets(context.Background())
}

func (u containerUpgrades) Abandon(up *instance.Upgrade) {}
