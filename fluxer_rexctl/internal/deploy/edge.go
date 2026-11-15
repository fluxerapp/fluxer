// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	caddyfileTarget = "/etc/caddy/Caddyfile"
	dropInTarget    = "/etc/caddy/conf.d/"
)

var dropInFile = regexp.MustCompile(regexp.QuoteMeta(dropInTarget) + `([^:\s/]+\.caddy)`)

type EdgeError struct {
	Caddy string
	Site  string
}

func (e *EdgeError) Error() string { return "caddy: " + e.Caddy }

func (e *EdgeError) Fix(then string) string {
	if e.Site != "" {
		return fmt.Sprintf("%s defines its own site. Move it to %s, then %s.", e.Site, instance.CaddySitesDir, then)
	}
	return fmt.Sprintf("Fix the file Caddy reports, then %s.", then)
}

func CheckEdge(ctx context.Context, inst *instance.Instance, p *engine.Project, caddyfile string) error {
	if !slices.Contains(p.Services(), edgeService) {
		return nil
	}
	o := p.HelperFor(edgeService)
	if o.Image == "" {
		return nil
	}
	if _, found, err := p.Engine.LocalImage(ctx, o.Image); err != nil || !found {
		return nil
	}
	if caddyfile != "" {
		for i, m := range o.Mounts {
			if m.Target == caddyfileTarget {
				o.Mounts[i].Bind = caddyfile
			}
		}
	}
	o.Entrypoint = []string{"caddy"}
	o.Command = []string{"adapt", "--config", caddyfileTarget, "--adapter", "caddyfile"}
	err := p.Helper(ctx, o)
	var exit *engine.ExitError
	if !errors.As(err, &exit) {
		return ctx.Err()
	}
	edge := edgeProblem(inst.Dir, exit.Stderr)
	if edge.Caddy == "" {
		edge.Caddy = exit.Error()
	}
	return &ui.ExitError{Code: ui.ExitRefused, Msg: "Caddy can't load the edge config.\n" + edge.Caddy, Fix: edge.Fix("try again"), Err: edge}
}

func edgeProblem(dir, stderr string) *EdgeError {
	msg := caddyMessage(stderr)
	e := &EdgeError{Caddy: strings.ReplaceAll(strings.ReplaceAll(msg, dropInTarget, instance.CaddyDropInDir+"/"), caddyfileTarget, "Caddyfile")}
	var files []string
	if m := dropInFile.FindStringSubmatch(msg); m != nil {
		files = []string{m[1]}
	} else if all, err := filepath.Glob(filepath.Join(dir, instance.CaddyDropInDir, "*.caddy")); err == nil {
		for _, f := range all {
			files = append(files, filepath.Base(f))
		}
	}
	for _, f := range files {
		data, err := os.ReadFile(filepath.Join(dir, instance.CaddyDropInDir, f))
		if err == nil && instance.DefinesSite(data) {
			e.Site = instance.CaddyDropInDir + "/" + f
			break
		}
	}
	return e
}

func caddyMessage(stderr string) string {
	var last string
	for _, line := range strings.Split(strings.TrimSpace(stderr), "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		var entry struct {
			Level string `json:"level"`
			Msg   string `json:"msg"`
		}
		if json.Unmarshal([]byte(line), &entry) == nil {
			if entry.Level == "error" || entry.Level == "fatal" {
				last = entry.Msg
			}
			continue
		}
		last = strings.TrimPrefix(line, "Error: ")
	}
	return strings.TrimPrefix(last, "adapting config using caddyfile: ")
}
