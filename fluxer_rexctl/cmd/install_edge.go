// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"cmp"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func edgeHolder(dir string) *instance.Instance {
	for _, o := range instance.Others(dir) {
		mode := o.State.Settings[settings.EdgeMode]
		if o.State.IsNative() && (mode == "" || mode == settings.EdgeBundled) {
			return o
		}
	}
	return nil
}

func edgeSitePath(edge *instance.Instance, project string) string {
	return edge.Path(instance.CaddySitesDir, "fluxer-"+project+".caddy")
}

func edgeSite(domain, bind string) []byte {
	tls := ""
	_, ip := settings.DomainIP(domain)
	switch {
	case checks.PrivateDomain(domain):
		tls = "\ttls internal\n"
	case ip:
		tls = "\ttls {\n\t\tissuer acme {\n\t\t\tprofile shortlived\n\t\t}\n\t\tissuer internal\n\t}\n"
	}
	return fmt.Appendf(nil, "%s {\n%s\treverse_proxy %s {\n\t\theader_up X-Forwarded-For {client_ip}\n\t\theader_up X-Real-IP {client_ip}\n\t\theader_up -Forwarded\n\t\tstream_close_delay 5m\n\t}\n}\n", domain, tls, bind)
}

func localAddresses() []netip.Addr {
	var out []netip.Addr
	addrs, _ := net.InterfaceAddrs()
	for _, a := range addrs {
		if n, ok := a.(*net.IPNet); ok {
			if ip, ok := netip.AddrFromSlice(n.IP); ok {
				out = append(out, ip.Unmap())
			}
		}
	}
	return out
}

func noSNIWarning(domain string, edge *instance.Instance, local []netip.Addr) (string, string) {
	ip, isIP := settings.DomainIP(domain)
	if !isIP || checks.PrivateDomain(domain) || slices.Contains(local, ip) {
		return "", ""
	}
	options := strings.TrimSpace(edge.Setting(globalOptionsVar))
	if strings.Contains(" "+strings.Join(strings.Fields(options), " ")+" ", " default_sni "+ip.String()+" ") {
		return "", ""
	}
	msg := fmt.Sprintf("Browsers that open https://%s send no server name, and %s isn't on a network interface of this machine, so the Fluxer in %s can't tell which certificate to answer with.", domain, ip, edge.Dir)
	if edgeIP, ok := settings.DomainIP(edge.Domain()); ok && !checks.PrivateDomain(edge.Domain()) {
		return msg, fmt.Sprintf("That Fluxer already sends those browsers the certificate for %s. Give this Fluxer a domain name, or move it to its own machine.", edgeIP)
	}
	if options != "" {
		return msg, fmt.Sprintf("%s holds one Caddy option and already holds %s, so it has no room for default_sni %s. Give this Fluxer a domain name, or move it to its own machine.", globalOptionsVar, options, ip)
	}
	return msg, "Run rexctl config set " + globalOptionsVar + " " + shellWord("default_sni "+ip.String()) + " --dir " + shellWord(edge.Dir) + "."
}

func putEdgeSite(edge *instance.Instance, project, domain, bind string) (bool, error) {
	path := edgeSitePath(edge, project)
	want := edgeSite(domain, bind)
	if have, err := os.ReadFile(path); err == nil && bytes.Equal(have, want) {
		return false, nil
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return false, err
	}
	return true, os.WriteFile(path, want, 0o644)
}

func (in *installer) routeThroughEdge(ctx context.Context, s *ui.Step) error {
	edge := in.net.edge
	fix := "Run rexctl install again, or put your own proxy in front of " + in.f.Bind + "."
	if _, err := putEdgeSite(edge, in.inst.Project(), in.f.Domain, in.f.Bind); err != nil {
		return ui.Wrap(err, "Couldn't add "+in.f.Domain+" to the Fluxer in "+edge.Dir+".", fix)
	}
	if err := rt.ReloadEdge(ctx, edge); err != nil {
		return ui.Wrap(err, "Couldn't reload the Fluxer in "+edge.Dir+".", "Run rexctl restart --dir "+shellWord(edge.Dir)+".")
	}
	if msg, fix := noSNIWarning(in.f.Domain, edge, localAddresses()); msg != "" {
		s.Warn(msg, fix)
	}
	s.Done("The Fluxer in %s now serves https://%s.", edge.Dir, in.f.Domain)
	return nil
}

func unrouteFromEdge(ctx context.Context, app *App, inst *instance.Instance) {
	dir := inst.State.ProxiedBy
	if dir == "" {
		return
	}
	edge, err := instance.Find(dir)
	if err != nil {
		return
	}
	if err := os.Remove(edgeSitePath(edge, inst.Project())); err != nil {
		if !errors.Is(err, fs.ErrNotExist) {
			app.UI.Detail("%v", err)
		}
		return
	}
	if err := rt.ReloadEdge(ctx, edge); err != nil {
		app.UI.Detail("reloading the Fluxer in %s: %v", edge.Dir, err)
	}
}

func routedThrough(inst *instance.Instance) []*instance.Instance {
	var out []*instance.Instance
	own := instance.RealPath(inst.Dir)
	for _, o := range instance.Others(inst.Dir) {
		if o.State.ProxiedBy != "" && instance.RealPath(o.State.ProxiedBy) == own {
			out = append(out, o)
		}
	}
	return out
}

func edgeGuard(inst *instance.Instance, changes map[string]*string) error {
	kind, kindSet := changes[settings.EdgeProxyKind]
	if inst.State.ProxiedBy == "" {
		if kindSet && kind != nil && *kind == settings.ProxyInstance {
			return ui.Usage("rexctl sets edge.proxy_kind to instance by itself, when it puts this Fluxer behind another one on this machine. Pass local, remote, tunnel or overlay.")
		}
		return nil
	}
	if mode, ok := changes[settings.EdgeMode]; ok && (mode == nil || *mode == settings.EdgeBundled) {
		return ui.Refused(fmt.Sprintf("The Fluxer in %s serves this one on ports 80 and 443, so this one can't serve HTTPS itself.", inst.State.ProxiedBy), "Keep it behind the Fluxer in "+inst.State.ProxiedBy+", or move it to its own machine with rexctl backup and rexctl restore.")
	}
	return nil
}

func syncEdgeSite(ctx context.Context, app *App, inst *instance.Instance) error {
	dir := inst.State.ProxiedBy
	if dir == "" {
		return nil
	}
	if inst.State.Settings[settings.EdgeProxyKind] != settings.ProxyInstance {
		unrouteFromEdge(ctx, app, inst)
		inst.State.ProxiedBy = ""
		return saveInstance(inst)
	}
	edge, err := instance.Find(dir)
	if err != nil {
		app.UI.Warn(fmt.Sprintf("rexctl can't find the Fluxer in %s, which serves this one.", dir), "Run rexctl proxy on with --proxy local and point your own proxy at this one.")
		return nil
	}
	bind, err := settings.NormaliseBind(cmp.Or(inst.State.Settings[settings.EdgeBind], settings.DefaultBind))
	if err != nil {
		return err
	}
	changed, err := putEdgeSite(edge, inst.Project(), inst.Domain(), bind)
	if err != nil {
		return ui.Wrap(err, "Couldn't update "+inst.Domain()+" in the Fluxer in "+edge.Dir+".", "Check that you can write to "+edge.Dir+", then run the same command again.")
	}
	if !changed {
		return nil
	}
	if err := rt.ReloadEdge(ctx, edge); err != nil {
		return ui.Wrap(err, "Couldn't reload the Fluxer in "+edge.Dir+".", "Run rexctl restart --dir "+shellWord(edge.Dir)+".")
	}
	app.UI.Info("The Fluxer in %s now serves https://%s through %s.", edge.Dir, inst.Domain(), bind)
	if msg, fix := noSNIWarning(inst.Domain(), edge, localAddresses()); msg != "" {
		app.UI.Warn(msg, fix)
	}
	return nil
}

func restoredEdge(app *App, inst *instance.Instance) error {
	sites, _ := filepath.Glob(inst.Path(instance.CaddySitesDir, "fluxer-*.caddy"))
	for _, f := range sites {
		if err := os.Remove(f); err != nil {
			return ui.Wrap(err, "Couldn't remove "+f+", which served another Fluxer on the backup's machine.", "Check that you can write to "+inst.Dir+", then try again.")
		}
	}
	st := inst.State
	mode := st.Settings[settings.EdgeMode]
	if st.ProxiedBy == "" && mode != "" && mode != settings.EdgeBundled {
		return nil
	}
	if edge := edgeHolder(inst.Dir); edge != nil {
		if busy, _ := busyWebPorts(); len(busy) > 0 {
			bind := st.Settings[settings.EdgeBind]
			if st.ProxiedBy == "" || !bindFree(bind) {
				bind = freeLoopbackBind()
			}
			st.ProxiedBy = edge.Dir
			st.Settings[settings.EdgeMode] = settings.EdgeProxy
			st.Settings[settings.EdgeBind] = bind
			st.Settings[settings.EdgeProxyKind] = settings.ProxyInstance
			app.UI.Info("The Fluxer in %s serves this one too, through %s.", edge.Dir, bind)
			return nil
		}
	}
	if st.ProxiedBy != "" {
		st.ProxiedBy = ""
		for _, k := range []string{settings.EdgeMode, settings.EdgeBind, settings.EdgeProxyKind} {
			delete(st.Settings, k)
		}
		app.UI.Info("The backup's Fluxer went through another Fluxer on its machine. This one serves HTTPS itself.")
	}
	return nil
}

func bindFree(bind string) bool {
	norm, err := settings.NormaliseBind(bind)
	if err != nil {
		return false
	}
	l, err := net.Listen("tcp", norm)
	if err != nil {
		return false
	}
	_ = l.Close()
	return true
}
