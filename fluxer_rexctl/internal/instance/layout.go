// SPDX-License-Identifier: AGPL-3.0-or-later

package instance

import (
	"context"
	"encoding/json"
	"errors"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/envfile"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const generatedSchema = 3

var (
	euid  = os.Geteuid
	chown = os.Lchown
)

func (i *Instance) OldLayout() bool {
	return i.State != nil && i.State.rootLayout
}

func (i *Instance) Generated(name string) string {
	return filepath.Join(stack.FilesDir(i.Dir, i.OldLayout()), filepath.FromSlash(name))
}

func (i *Instance) StackFiles(overlays []stack.Overlay) []string {
	return stack.ComposeFilesAt(i.Dir, overlays, i.OldLayout())
}

func (i *Instance) Render(set *stack.Set, overlays []stack.Overlay, images map[string]string) (*stack.Rendered, error) {
	return stack.RenderAt(set, overlays, images, i.Project(), i.Dir, i.OldLayout())
}

func (i *Instance) Migrate(prepare func() error) error {
	if !i.OldLayout() || i.State.Ejected != nil {
		return nil
	}
	schema, err := savedSchema(i.StatePath())
	if err != nil {
		return err
	}
	if schema >= generatedSchema {
		i.State.rootLayout = false
		return nil
	}
	if prepare != nil {
		if err := prepare(); err != nil {
			return err
		}
	}
	left, err := copyGenerated(i)
	if err != nil {
		return err
	}
	i.State.rootLayout = false
	i.State.LeftInRoot = left
	if err := i.Save(); err != nil {
		i.State.rootLayout, i.State.LeftInRoot = true, nil
		return err
	}
	return i.DropRootCopies(context.Background(), false)
}

func savedSchema(path string) (int, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return 0, err
	}
	var head struct {
		Schema int `json:"schema"`
	}
	return head.Schema, json.Unmarshal(data, &head)
}

func copyGenerated(i *Instance) (map[string]string, error) {
	gen := stack.GeneratedPath(i.Dir)
	if err := os.MkdirAll(gen, 0o755); err != nil {
		return nil, err
	}
	if err := i.giveToOwner(gen); err != nil {
		return nil, err
	}
	left := map[string]string{}
	for _, name := range stack.FileNames() {
		src := filepath.Join(i.Dir, name)
		info, err := os.Lstat(src)
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return nil, err
		}
		if !info.Mode().IsRegular() {
			continue
		}
		data, err := os.ReadFile(src)
		if err != nil {
			return nil, err
		}
		dst := filepath.Join(gen, name)
		if err := envfile.WriteFileAtomic(dst, data, false); err != nil {
			return nil, err
		}
		if err := i.giveToOwner(dst); err != nil {
			return nil, err
		}
		left[name] = stack.Hash(data)
	}
	return left, nil
}

func (i *Instance) DropRootCopies(ctx context.Context, started bool) error {
	if i.State == nil || i.OldLayout() || i.State.Ejected != nil || len(i.State.LeftInRoot) == 0 {
		return nil
	}
	changed := false
	for name, hash := range i.State.LeftInRoot {
		_, mounted := stack.MountedFiles[name]
		if mounted && (!started || i.rootMounted(ctx, name)) {
			continue
		}
		data, err := os.ReadFile(i.Path(name))
		switch {
		case errors.Is(err, fs.ErrNotExist):
		case err != nil:
			return err
		case stack.Hash(data) == hash:
			if err := os.Remove(i.Path(name)); err != nil {
				return err
			}
		case mounted:
			continue
		}
		delete(i.State.LeftInRoot, name)
		changed = true
	}
	if !changed {
		return nil
	}
	return i.Save()
}

func (i *Instance) UnusedRootFiles(ctx context.Context) []string {
	if i.State == nil || i.OldLayout() || i.State.Ejected != nil {
		return nil
	}
	var out []string
	for _, name := range slices.Sorted(maps.Keys(i.State.LeftInRoot)) {
		if _, mounted := stack.MountedFiles[name]; !mounted {
			continue
		}
		data, err := os.ReadFile(i.Path(name))
		if err != nil || stack.Hash(data) == i.State.LeftInRoot[name] || i.rootMounted(ctx, name) {
			continue
		}
		out = append(out, name)
	}
	return out
}

func (i *Instance) rootMounted(ctx context.Context, name string) bool {
	compose, err := os.ReadFile(i.Generated(stack.ComposeFile))
	if err != nil || !stack.MountsGenerated(compose, name) {
		return true
	}
	custom := i.Path(CustomCompose)
	if _, err := os.Stat(custom); errors.Is(err, fs.ErrNotExist) {
		return false
	}
	sources, err := engine.BindSources(ctx, i.Dir, i.Project(), []string{custom}, map[string]string{"PWD": i.Dir})
	if err != nil {
		return true
	}
	return slices.Contains(sources, filepath.Clean(i.Path(name)))
}
