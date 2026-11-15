// SPDX-License-Identifier: AGPL-3.0-or-later

package migrate

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"maps"
	"path/filepath"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

const (
	ImporterService = "api"
	ImporterScript  = "dist/MigrateEntrypoint.js"
	ImportKV        = "kv"
	ImportStorage   = "storage"

	ObjectsGroup = "10001"
)

type Job struct {
	Args    []string
	Script  string
	EnvArgs [][2]string
	Objects bool
	Stdout  io.Writer
	Stderr  io.Writer
}

type Importer interface {
	Run(ctx context.Context, j Job) error
}

func (j Job) argv(node, script string) []string {
	base := []string{node}
	if j.Script != "" {
		base = append(base, "-e", j.Script)
	} else {
		base = append(base, script)
	}
	base = append(base, j.Args...)
	if len(j.EnvArgs) == 0 {
		return base
	}
	var tail strings.Builder
	tail.WriteString(`exec "$@"`)
	for _, a := range j.EnvArgs {
		fmt.Fprintf(&tail, ` %s "$%s"`, a[0], a[1])
	}
	return append([]string{"sh", "-c", tail.String(), "sh"}, base...)
}

type ComposeImporter struct {
	Project *engine.Project
	Objects *engine.Project
}

var ErrNoImporterNetwork = errors.New("migrate: the api service has no network to run the importer on")

func (c ComposeImporter) Run(ctx context.Context, j Job) error {
	p := c.Project
	if j.Objects && c.Objects != nil {
		p = c.Objects
	}
	o := p.HelperFor(ImporterService)
	o.Network = network(p)
	if o.Network == "" {
		return ErrNoImporterNetwork
	}
	if j.Objects {
		o.Mounts = append(o.Mounts, engine.Mount{Volume: stack.ObjectsVolume, Target: stack.ObjectsRoot})
		o.GroupAdd = append(o.GroupAdd, ObjectsGroup)
	}
	o.Command = j.argv("node", ImporterScript)
	o.Stdout, o.Stderr = j.Stdout, j.Stderr
	return p.Helper(ctx, o)
}

func network(p *engine.Project) string {
	m := p.Model
	if m == nil {
		return ""
	}
	svc, ok := m.Services[ImporterService]
	if !ok {
		return ""
	}
	for _, key := range slices.Sorted(maps.Keys(svc.Networks)) {
		if n, ok := m.Networks[key]; ok && n.Name != "" {
			return n.Name
		}
	}
	return ""
}

type NativeImporter struct {
	Stack *native.Stack
	Set   *stack.Set
}

func (n NativeImporter) Run(ctx context.Context, j Job) error {
	root := n.Stack.Current()
	cmd := j.argv(filepath.Join(root, "bin", "node"), filepath.Join(root, ImporterService, ImporterScript))
	return n.Stack.Exec(ctx, n.Set, ImporterService, engine.ExecOptions{Stdout: j.Stdout, Stderr: j.Stderr}, cmd...)
}

type tail struct{ buf []byte }

func (t *tail) Write(p []byte) (int, error) {
	t.buf = append(t.buf, p...)
	if len(t.buf) > 8<<10 {
		t.buf = t.buf[len(t.buf)-8<<10:]
	}
	return len(p), nil
}

func (t *tail) last() string {
	lines := strings.Split(strings.TrimSpace(string(t.buf)), "\n")
	return strings.TrimSpace(lines[len(lines)-1])
}

func runJSON(ctx context.Context, imp Importer, j Job, out any) error {
	var stdout bytes.Buffer
	errs := &tail{}
	j.Stdout = &stdout
	j.Stderr = io.MultiWriter(errs, orDiscard(j.Stderr))
	runErr := imp.Run(ctx, j)
	lines := strings.Split(strings.TrimSpace(stdout.String()), "\n")
	last := strings.TrimSpace(lines[len(lines)-1])
	if last != "" && json.Unmarshal([]byte(last), out) == nil {
		return nil
	}
	if runErr == nil {
		return fmt.Errorf("the importer printed no report")
	}
	if msg := errs.last(); msg != "" {
		return fmt.Errorf("%w: %s", runErr, msg)
	}
	return runErr
}

func orDiscard(w io.Writer) io.Writer {
	if w == nil {
		return io.Discard
	}
	return w
}
