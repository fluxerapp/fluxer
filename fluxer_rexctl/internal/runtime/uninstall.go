// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/proxyconf"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func exists(path string) bool {
	_, err := os.Lstat(path)
	return err == nil
}

func dirSize(dir string) int64 {
	var total int64
	_ = filepath.WalkDir(dir, func(_ string, d fs.DirEntry, err error) error {
		if err != nil {
			return nil
		}
		if d.Type().IsRegular() {
			if info, err := d.Info(); err == nil {
				total += info.Size()
			}
		}
		return nil
	})
	return total
}

func sized(path string) string {
	if n := dirSize(path); n > 0 {
		return fmt.Sprintf("%s (%s)", path, ui.Bytes(n))
	}
	return path
}

func folderRow(dir string) ui.KV {
	return ui.KV{Key: "Folder", Value: sized(dir)}
}

func inside(path, dir string) bool {
	rel, err := filepath.Rel(dir, path)
	return err == nil && rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator))
}

func folderRows(inst *instance.Instance, o UninstallOptions, goes, stays []ui.KV) ([]ui.KV, []ui.KV) {
	if o.KeepBackups && exists(inst.BackupDir()) && inside(inst.BackupDir(), inst.Dir) {
		return goes, append(stays, ui.KV{Key: "Folder", Value: inst.Dir + ", which keeps the backups"})
	}
	return append(goes, ui.KV{Key: "Folder", Value: inst.Dir}), stays
}

func backupRows(inst *instance.Instance, o UninstallOptions, goes, stays []ui.KV) ([]ui.KV, []ui.KV) {
	dir := inst.BackupDir()
	if !exists(dir) {
		return goes, stays
	}
	row := ui.KV{Key: "Backups", Value: sized(dir)}
	if o.KeepData || o.KeepBackups || !inside(dir, inst.Dir) {
		return goes, append(stays, row)
	}
	return append(goes, row), stays
}

func addedRules(inst *instance.Instance) []instance.OpenedPort {
	out := slices.Clone(inst.State.Firewall)
	var specs []string
	for _, v := range inst.State.Voice {
		specs = append(specs, v.Opened...)
	}
	if e := inst.State.VoiceEdge; e != nil {
		specs = append(specs, e.Opened...)
	}
	for _, spec := range specs {
		num, proto, ok := strings.Cut(spec, "/")
		n, err := strconv.Atoi(num)
		if !ok || err != nil || slices.ContainsFunc(out, func(p instance.OpenedPort) bool { return p.Number == n && p.Proto == proto && p.Source == "" }) {
			continue
		}
		out = append(out, instance.OpenedPort{Number: n, Proto: proto})
	}
	return out
}

func proxyRows() []ui.KV {
	var found []string
	for _, p := range proxyconf.EnabledPaths {
		if exists(p) {
			found = append(found, p)
		}
	}
	if len(found) == 0 {
		return nil
	}
	return []ui.KV{{Key: "Proxy", Value: "Your web server's config in " + ui.JoinAnd(found)}}
}

func firewallRows(inst *instance.Instance) []ui.KV {
	rules := addedRules(inst)
	if len(rules) == 0 {
		return nil
	}
	ports := make([]string, 0, len(rules))
	for _, p := range rules {
		ports = append(ports, fmt.Sprintf("%d/%s", p.Number, p.Proto))
	}
	return []ui.KV{{Key: "Firewall", Value: "The rules for " + ui.JoinAnd(ports) + " that rexctl added"}}
}

func closeFirewall(ctx context.Context, inst *instance.Instance) []string {
	rules := addedRules(inst)
	byFirewall := map[host.Firewall][]host.Port{}
	var order []host.Firewall
	var detected *host.Firewall
	for _, p := range rules {
		fw := host.Firewall(p.Firewall)
		if fw == host.FirewallNone {
			if detected == nil {
				d, _ := host.DetectFirewall(ctx)
				detected = &d
			}
			if fw = *detected; fw == host.FirewallNone {
				continue
			}
		}
		if _, seen := byFirewall[fw]; !seen {
			order = append(order, fw)
		}
		byFirewall[fw] = append(byFirewall[fw], host.Port{Number: p.Number, Proto: p.Proto, Source: p.Source})
	}
	var left []string
	for _, fw := range order {
		if err := host.ClosePorts(ctx, fw, byFirewall[fw]); err != nil {
			ports := make([]string, 0, len(byFirewall[fw]))
			for _, p := range byFirewall[fw] {
				ports = append(ports, p.String())
			}
			left = append(left, fmt.Sprintf("The %s rules for %s are still on this machine.", fw, ui.JoinAnd(ports)))
		}
	}
	return left
}

type Removal struct {
	Goes    []ui.KV
	Stays   []ui.KV
	Account string
	Kept    string
}

func PlanUninstall(ctx context.Context, r Runtime, o UninstallOptions) Removal {
	switch r := r.(type) {
	case *compose:
		return r.plan(ctx, o)
	case *nativeRT:
		return r.plan(o)
	case *containerRT:
		return r.plan(o)
	}
	return Removal{}
}

func DataLeft(r Runtime) bool {
	switch r := r.(type) {
	case *compose:
		return r.dataLeft
	case *nativeRT:
		return r.dataLeft
	}
	return false
}

func UninstallEngine(r Runtime) *engine.Engine {
	if c, ok := r.(*compose); ok {
		return c.e
	}
	return nil
}

func BootSteps(ctx context.Context, r Runtime, enable bool) ([]host.Step, error) {
	switch b := r.Boot().(type) {
	case composeBoot:
		return b.steps(ctx, enable)
	case nativeBoot:
		return b.steps(ctx, enable)
	case containerBoot:
		return b.steps(ctx, enable)
	}
	return nil, nil
}

func stillThere(s *ui.Step, left []string) {
	if s == nil {
		return
	}
	for _, item := range left {
		s.Warn(item, "")
	}
}
