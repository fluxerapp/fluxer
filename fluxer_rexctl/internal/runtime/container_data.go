// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"slices"
	"strconv"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	dataFrom     = "/from"
	dataTo       = "/to"
	dataHeadroom = 110
)

var errDataCopy = errors.New("runtime: the copy of Fluxer's data doesn't match the original")

func DataFolder(inst *instance.Instance) string {
	if inst == nil || inst.State == nil {
		return ""
	}
	return strings.TrimSpace(inst.State.Settings[settings.DataFolder])
}

func dataWhere(inst *instance.Instance) string {
	if folder := DataFolder(inst); folder != "" {
		return folder
	}
	return VolumeName(inst)
}

func DataPlace(inst *instance.Instance) string {
	return dataPlace(inst, DataFolder(inst))
}

func dataPlace(inst *instance.Instance, folder string) string {
	if folder != "" {
		return "the folder " + folder
	}
	return "the volume " + VolumeName(inst)
}

func mountData(inst *instance.Instance, s *engine.SingleSpec, folder, target string) {
	if folder != "" {
		s.Binds = append(s.Binds, folder+":"+target+":z")
		return
	}
	s.Volume, s.VolumeTarget = VolumeName(inst), target
}

func (r *containerRT) dataHelper(from, to string, script string) engine.SingleSpec {
	s := engine.SingleSpec{
		Name:       r.name() + "-data-" + strconv.FormatInt(time.Now().UnixNano(), 36),
		Image:      ImageRef(r.inst.State.Release),
		Entrypoint: []string{"sh", "-c"},
		Cmd:        []string{script},
		User:       "0:0",
		Labels:     map[string]string{LabelManagedBy: ManagedByRexctl},
	}
	mountData(r.inst, &s, from, dataFrom)
	if to != from {
		mountData(r.inst, &s, to, dataTo)
	}
	return s
}

const countScript = `count() { find "$1" -mindepth 1 ! -path "$1/lost+found" ! -path "$1/lost+found/*" | wc -l; find "$1" -type f ! -path "$1/lost+found/*" -printf '%s\n' | awk '{s += $1} END {printf "%d\n", s}'; }; `

func copyScript() string {
	id := strconv.Itoa(ContainerUID)
	return "set -e; " + countScript +
		`cp -a ` + dataFrom + `/. ` + dataTo + `/; chown ` + id + `:` + id + ` ` + dataTo + `; chmod 0700 ` + dataTo + `; ` +
		`count ` + dataFrom + `; count ` + dataTo
}

const clearScript = `find ` + dataFrom + ` -mindepth 1 -maxdepth 1 ! -name lost+found -exec rm -rf {} +`

func (r *containerRT) dataUsage(ctx context.Context, e containerEngine) (int64, error) {
	var out bytes.Buffer
	from := DataFolder(r.inst)
	if err := e.RunOnce(ctx, r.dataHelper(from, from, "du -sk "+dataFrom), nil, &out, nil); err != nil {
		return 0, err
	}
	kb, err := strconv.ParseInt(strings.Fields(out.String() + " x")[0], 10, 64)
	if err != nil {
		return 0, err
	}
	return kb * 1024, nil
}

type dataCount struct {
	entries, bytes int64
}

func parseCounts(out string) (from, to dataCount, err error) {
	var n []int64
	for _, f := range strings.Fields(out) {
		v, err := strconv.ParseInt(f, 10, 64)
		if err != nil {
			return from, to, fmt.Errorf("%w: %q", errDataCopy, out)
		}
		n = append(n, v)
	}
	if len(n) != 4 {
		return from, to, fmt.Errorf("%w: %q", errDataCopy, out)
	}
	return dataCount{n[0], n[1]}, dataCount{n[2], n[3]}, nil
}

func (c containerSettings) dataProblem(ctx context.Context, p *deploy.Prepared) error {
	r := c.r
	to := strings.TrimSpace(p.Settings[settings.DataFolder])
	if to == DataFolder(r.inst) {
		return nil
	}
	if len(p.Keys) > 1 {
		return ui.Usage("Change data.folder on its own, without other settings.")
	}
	if from := DataFolder(r.inst); from != "" && to != "" && (inside(to, from) || inside(from, to)) {
		return ui.Refused(fmt.Sprintf("%s and %s are inside each other, and Fluxer's data needs a folder of its own.", from, to), "Pick a folder outside "+from+".")
	}
	if r.inst.State.Upgrade != nil {
		return ui.Refused("An upgrade of this instance didn't finish.", "Run rexctl upgrade to finish it, then try again.")
	}
	e, err := r.engine(ctx)
	if err != nil {
		return err
	}
	if to == "" {
		taken, err := e.VolumeExists(ctx, VolumeName(r.inst))
		if err != nil {
			return ContainerError(err, "Couldn't check the volume "+VolumeName(r.inst)+".")
		}
		if taken {
			return ui.Refused(fmt.Sprintf("The volume %s still holds an older copy of Fluxer's data.", VolumeName(r.inst)),
				fmt.Sprintf("Check that you don't need it, remove it with %s volume rm %s, then try again.", r.kindTool(), VolumeName(r.inst)))
		}
	} else if msg, fix := checks.NewDataFolderProblem(to, r.inst.State.ForcedFolder == to); msg != "" {
		return ui.Refused(msg, fix)
	}
	used, err := r.dataUsage(ctx, e)
	if err != nil {
		return ContainerError(err, "Couldn't measure Fluxer's data.")
	}
	where := to
	if where == "" {
		where = engineStores(e, r.inst).Data
	}
	free, err := backup.FreeSpace(where)
	if err != nil {
		return nil
	}
	if need := uint64(used) * dataHeadroom / 100; free < need {
		return ui.Refused(fmt.Sprintf("%s has %s free, and Fluxer's data needs %s there.", where, ui.Bytes(int64(free)), ui.Bytes(int64(need))),
			"Free up space on that disk, or pick a folder on a bigger disk.")
	}
	return nil
}

func engineStores(e containerEngine, inst *instance.Instance) engine.Roots {
	eng, _ := e.(*engine.Engine)
	return engine.Stores(inst.Dir, eng, "")
}

func (r *containerRT) kindTool() string {
	if r.kind == engine.Podman {
		return "podman"
	}
	return "docker"
}

func (c containerSettings) moveData(ctx context.Context, s *ui.Step, p *deploy.Prepared) (*deploy.Change, error) {
	r := c.r
	st := r.inst.State
	from, to := DataFolder(r.inst), strings.TrimSpace(p.Settings[settings.DataFolder])
	e, err := r.engine(ctx)
	if err != nil {
		return nil, err
	}
	cur, ok, err := e.Inspect(ctx, r.name())
	if err != nil {
		return nil, ContainerError(err, "Couldn't check the state of Fluxer's container.")
	}
	running := ok && cur.Running() && !st.Stopped
	if ok {
		s.Title("Stopping Fluxer")
		if err := e.StopSingle(ctx, r.name(), ContainerStop); err != nil {
			return nil, ContainerError(err, "Couldn't stop Fluxer.")
		}
	}
	bg := context.WithoutCancel(ctx)
	back := func(cause error, msg string) (*deploy.Change, error) {
		if err := r.clearData(bg, e, to); err != nil {
			r.u.Detail("removing the copy in %s: %v", dataPlace(r.inst, to), err)
		}
		if c, ok, err := e.Inspect(bg, r.name()); running && err == nil && ok && !c.Running() {
			if err := e.StartSingle(bg, r.name()); err != nil {
				r.u.Detail("starting Fluxer again: %v", err)
			} else if err := r.wait(bg, s, "Starting Fluxer again"); err != nil {
				r.u.Detail("starting Fluxer again: %v", err)
			}
		}
		return nil, ui.Wrap(cause, msg, "Run the command again with --verbose to see why.")
	}
	if err := r.makeData(ctx, e, to); err != nil {
		return back(err, fmt.Sprintf("Couldn't create %s.", dataPlace(r.inst, to)))
	}
	s.Title("Copying Fluxer's data to " + dataPlace(r.inst, to))
	var out bytes.Buffer
	if err := e.RunOnce(ctx, r.dataHelper(from, to, copyScript()), nil, &out, stepLog{s}); err != nil {
		return back(err, "Couldn't copy Fluxer's data. It stays where it was.")
	}
	was, now, err := parseCounts(out.String())
	if err == nil && was != now {
		err = fmt.Errorf("%w: %d files and folders with %d bytes, and the copy has %d with %d bytes", errDataCopy, was.entries, was.bytes, now.entries, now.bytes)
	}
	if err != nil {
		return back(err, "The copy of Fluxer's data didn't match the original. The data stays where it was.")
	}
	s.Item("copy", fmt.Sprintf("Copied %d files and folders (%s)", now.entries, ui.Bytes(now.bytes)), ui.StateOK, "")
	oldSettings, oldSecrets := st.Settings, st.Secrets
	st.Settings, st.Secrets = p.Settings, p.Secrets
	if err := r.startData(ctx, s, running); err != nil {
		st.Settings, st.Secrets = oldSettings, oldSecrets
		if rerr := r.startData(bg, s, running); rerr != nil {
			err = errors.Join(err, rerr)
		}
		return back(err, fmt.Sprintf("Fluxer didn't start from %s. rexctl put it back on %s.", dataPlace(r.inst, to), dataPlace(r.inst, from)))
	}
	r.u.Info("%s", OldDataLine(r.inst, from, r.kindTool()))
	return &deploy.Change{Keys: p.Preview.Keys, Applied: true, Restarted: []string{"all"}}, nil
}

func OldDataLine(inst *instance.Instance, from, tool string) string {
	if from == "" {
		return fmt.Sprintf("The old copy of the data stays in the volume %s. Remove it with %s volume rm %s once Fluxer works from its new folder.", VolumeName(inst), tool, VolumeName(inst))
	}
	return fmt.Sprintf("The old copy of the data stays in %s. Delete that folder once Fluxer works from its new place.", from)
}

func (r *containerRT) dataGone() error {
	folder := DataFolder(r.inst)
	if folder == "" || r.inst.State.Installing {
		return nil
	}
	entries, err := os.ReadDir(folder)
	if err != nil && !errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if slices.ContainsFunc(entries, func(d os.DirEntry) bool { return d.Name() != "lost+found" }) {
		return nil
	}
	return ui.Refused(fmt.Sprintf("Fluxer's data folder %s is empty, and Fluxer would start there as a new instance.", folder),
		"If it's on another disk, mount that disk there again, then try again.")
}

func (r *containerRT) makeData(ctx context.Context, e containerEngine, folder string) error {
	if folder == "" {
		return e.CreateVolume(ctx, VolumeName(r.inst), composeVolumeLabels(r.inst.Project(), VolumeName(r.inst)))
	}
	if err := os.MkdirAll(folder, 0o700); err != nil {
		return err
	}
	if uid, _, ok := host.FileOwner(folder); ok && uid == ContainerUID {
		return nil
	}
	id := strconv.Itoa(ContainerUID)
	return e.RunOnce(ctx, r.dataHelper(folder, folder, "chown "+id+":"+id+" "+dataFrom+" && chmod 0700 "+dataFrom), nil, nil, nil)
}

func (r *containerRT) clearData(ctx context.Context, e containerEngine, folder string) error {
	if folder == "" {
		return e.RemoveVolume(ctx, VolumeName(r.inst))
	}
	if _, err := os.Stat(folder); err != nil {
		return nil
	}
	return e.RunOnce(ctx, r.dataHelper(folder, folder, clearScript), nil, nil, nil)
}

func (r *containerRT) startData(ctx context.Context, s *ui.Step, running bool) error {
	mode := applyChanges
	if !running {
		mode = applyCreate
	}
	if _, err := r.apply(ctx, mode); err != nil {
		return err
	}
	if !running {
		r.inst.State.Stopped = true
		return saveState(r.inst)
	}
	return r.wait(ctx, s, "Starting Fluxer")
}
