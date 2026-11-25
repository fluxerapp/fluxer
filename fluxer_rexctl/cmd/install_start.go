// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"maps"
	"strconv"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/proxyconf"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/setup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func (in *installer) tasks() []ui.Task {
	in.started = time.Now()
	var out []ui.Task
	if in.resuming == nil {
		_, pre := in.planned()
		out = append(out, pre...)
		out = append(out, ui.Task{ID: "save", Title: "Saving Fluxer and its settings", Run: in.save})
	}
	if in.native {
		out = append(out, in.nativeTasks()...)
	} else {
		out = append(out, in.containerTasks()...)
	}
	if !in.f.NoStart && !in.f.BehindProxy {
		out = append(out, ui.Task{ID: "certificate", Title: "Getting a certificate for " + in.f.Domain, Optional: true, Run: in.waitCertificate})
	}
	switch {
	case in.f.BehindProxy && in.f.Proxy == settings.ProxyInstance && in.net.edge != nil:
		out = append(out, ui.Task{ID: "proxy", Title: "Adding " + in.f.Domain + " to the Fluxer in " + in.net.edge.Dir, Optional: true, Run: in.routeThroughEdge})
	case in.f.BehindProxy:
		out = append(out, ui.Task{ID: "proxy", Title: "Writing configs for your proxy", Optional: true, Run: in.writeProxyConfigs})
	}
	last := &out[len(out)-1]
	run, skip := last.Run, last.Skip
	last.Skip = func() bool {
		if skip != nil && skip() {
			in.done = true
			return true
		}
		return false
	}
	last.Run = func(ctx context.Context, s *ui.Step) error {
		err := run(ctx, s)
		in.done = true
		return err
	}
	return out
}

func (in *installer) save(ctx context.Context, s *ui.Step) error {
	inst, err := instance.Create(in.dir, in.f.Project)
	if err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't create %s.", in.dir), "Check that you can write to it, or pass --dir.")
	}
	inst.State.Installing = true
	inst.State.Runtime = instance.RuntimeContainer
	if in.native {
		inst.State.Runtime = instance.RuntimeNative
	}
	l, err := inst.Lock(ctx)
	if err != nil {
		return ui.Refused("Another rexctl command is changing this instance.", "Wait for it to finish, then try again.")
	}
	in.inst = inst
	in.app.held = inst
	in.release = func() {
		in.app.held = nil
		giveFilesBack(in.app, inst)
		l.Release()
	}
	in.writeCommon(inst)
	if in.native {
		err = in.writeNative(inst)
	} else {
		err = in.writeContainer(inst)
	}
	if err != nil {
		return err
	}
	in.app.remember(inst.Dir)
	s.Done("Saved Fluxer and its settings in %s.", inst.Dir)
	return nil
}

func (in *installer) writeCommon(inst *instance.Instance) {
	f := in.f
	st := inst.State
	st.Settings[settings.Domain] = normaliseDomain(f.Domain)
	maps.Copy(st.Settings, in.networkSettings())
	if f.BehindProxy && f.Proxy == settings.ProxyInstance && in.net.edge != nil {
		st.ProxiedBy = in.net.edge.Dir
	}
	if f.VoiceTCPPort > 0 {
		st.Settings[settings.VoiceTCPPort] = strconv.Itoa(f.VoiceTCPPort)
	}
	if f.VoiceUDPPort > 0 {
		st.Settings[settings.VoiceUDPPort] = strconv.Itoa(f.VoiceUDPPort)
	}
	voiceIP := f.VoiceIP
	if voiceIP == "" && checks.TailnetDomain(f.Domain) {
		voiceIP = in.tailnetInfo(in.ctx).IPv4
	}
	if voiceIP != "" {
		st.Settings[settings.VoicePublicIP] = voiceIP
	}
	if f.NoVoice {
		st.Settings[settings.VoiceBundled] = "false"
	}
	if f.MemoryProfile != "" {
		st.Settings[settings.MemoryProfile] = f.MemoryProfile
	}
	if in.storage != nil {
		maps.Copy(st.Settings, in.storage.values())
	}
	data := inst.Dir
	if in.engine != nil {
		data = engine.Stores(inst.Dir, in.engine, "").Data
	}
	if total, err := host.DiskSize(data); err == nil {
		if profile := stack.DiskProfileFor(total); profile != "" {
			st.Settings[settings.DiskProfile] = profile
		}
	}
	st.BootService = bootValue(in.net.boot || in.rootlessPodman())
	st.Firewall = append(st.Firewall, in.opened...)
}

func (in *installer) waitCertificate(ctx context.Context, s *ui.Step) error {
	c, ok := checks.Get("certificate")
	if !ok {
		return nil
	}
	inst := in.inst
	env := &checks.Env{Dir: inst.Dir, Domain: inst.Domain(), Instance: inst, Host: in.host, Native: true, Container: in.container}
	deadline := time.Now().Add(3 * time.Minute)
	for {
		last := checks.RunIn(ctx, env, checks.Installed, c)
		switch {
		case last.Status == checks.Skip:
			s.Done("Fluxer uses its own certificate for %s.", inst.Domain())
			return nil
		case last.Status == checks.Pass:
			s.Done("Got a certificate for %s.", inst.Domain())
			return nil
		case time.Now().After(deadline):
			in.noCert = true
			return ui.Failed(last.Message, last.Fix)
		}
		s.Waiting("Getting a certificate for "+inst.Domain(), "Let's Encrypt")
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(5 * time.Second):
		}
	}
}

func (in *installer) writeProxyConfigs(ctx context.Context, s *ui.Step) error {
	files, err := proxyconf.Write(in.inst.Dir, proxyParams(in.f.Domain, in.f.Bind))
	if err != nil {
		return ui.Wrap(err, "Couldn't write the proxy configs.", "Run rexctl proxy config "+string(in.net.server)+" to print one.")
	}
	in.proxyFiles = files
	s.Done("Wrote configs for nginx, Caddy, Apache, Traefik and Cloudflare Tunnel to %s.", in.inst.Path(proxyconf.Dir))
	return nil
}

func refreshBootUnit(ctx context.Context, app *App, inst *instance.Instance, e *engine.Engine, p *engine.Project) error {
	u, err := checks.BootUnitFor(inst, e, p)
	if err != nil {
		return err
	}
	steps, err := u.RefreshSteps(ctx)
	if err != nil || len(steps) == 0 {
		return err
	}
	ok, err := confirmHostSteps(app, steps, "Update the service that starts Fluxer with this machine?")
	if err != nil {
		return err
	}
	if !ok {
		app.UI.Warn("The service that starts Fluxer with this machine still uses the old rexctl.", "Run rexctl doctor --fix to update it.")
		return nil
	}
	return host.RunSteps(ctx, steps)
}

func (in *installer) review() ui.Card {
	f := in.f
	rows := []ui.KV{
		{Key: "Address", Value: f.Domain, Edit: 'a'},
	}
	rows = append(rows, in.runtimeRow(), in.networkRow())
	rows = append(rows, ui.KV{Key: "Storage", Value: in.storageSummary(), Edit: 's'})
	switch {
	case f.NoStart:
		rows = append(rows, ui.KV{Key: "Admin", Value: "Later, after rexctl start"})
	case f.SetupWhere == "terminal":
		rows = append(rows, ui.KV{Key: "Admin", Value: in.admin.summary() + ", set up here in the terminal", Edit: 'u'})
	default:
		rows = append(rows, ui.KV{Key: "Admin", Value: "In your browser with a setup token", Edit: 'u'})
	}
	rows = append(rows, ui.KV{Key: "Size", Value: sizingLine(f.MemoryProfile)})
	var plan []ui.PlanItem
	if in.resuming == nil {
		plan = append(plan, ui.PlanItem{Verb: ui.PlanCreate, Text: "Create " + in.dir})
	}
	plan = append(plan, ui.PlanItem{Verb: ui.PlanRun, Text: "Download Fluxer " + in.releaseLabel()})
	items, _ := in.planned()
	plan = append(plan, items...)
	switch {
	case f.BehindProxy && f.Proxy == settings.ProxyInstance && in.net.edge != nil:
		plan = append(plan, ui.PlanItem{Verb: ui.PlanChange, Text: "Serve plain HTTP on " + f.Bind + " and let the Fluxer in " + in.net.edge.Dir + " serve https://" + f.Domain})
	case f.BehindProxy:
		plan = append(plan, ui.PlanItem{Verb: ui.PlanChange, Text: "Serve plain HTTP on " + f.Bind + " and leave HTTPS to your " + proxyNoun(f.Proxy)})
		plan = append(plan, ui.PlanItem{Verb: ui.PlanCreate, Text: "Write " + proxyconf.Path(in.dir, proxyServer(f.Proxy, in.net.server)) + " for you to enable"})
	}
	if f.NoStart {
		plan = append(plan, ui.PlanItem{Verb: ui.PlanKeep, Text: "Leave Fluxer stopped"})
	} else {
		plan = append(plan, ui.PlanItem{Verb: ui.PlanRun, Text: "Start Fluxer at " + in.origin()})
	}
	title := ui.RexPlanTitle
	if f.DryRun {
		title = "What rexctl would do"
	}
	return ui.Card{Title: title, Rows: rows, Plan: plan}
}

func (in *installer) origin() string {
	if in.inst != nil {
		return in.inst.Origin()
	}
	scheme := "https"
	if in.plainHTTP() {
		scheme = "http"
	}
	return scheme + "://" + in.f.Domain
}

func (in *installer) finish(ctx context.Context) error {
	u := in.app.UI
	inst := in.inst
	origin := inst.Origin()
	token := setup.Token(inst.State.Secrets)
	if token == "" && !inst.State.SetupDone {
		t, err := setup.EnsureToken(inst.State.Secrets)
		if err != nil {
			return err
		}
		if err := inst.Save(); err != nil {
			return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then run rexctl install again.")
		}
		token = t
	}
	if in.f.NoStart {
		return in.finishStopped(token)
	}
	if in.f.BehindProxy {
		if in.f.Proxy != settings.ProxyInstance {
			in.proxyCard()
		}
		if err := in.waitProxy(ctx); err != nil {
			return err
		}
	}
	client, err := in.app.instanceAdmin(ctx, inst)
	if err != nil {
		return err
	}
	root := saveRootCert(ctx, in.app, in.inst, 20*time.Second)
	cert := ""
	if root != nil {
		cert = root.path
	}
	if done, err := setup.Done(ctx, client); err == nil && done {
		inst.State.SetupDone = true
		if err := inst.Save(); err != nil {
			return err
		}
		rootCertCard(u, root, origin)
		u.Success(in.success(origin, cert, nil))
		return nil
	}
	if in.f.SetupWhere != "terminal" {
		rootCertCard(u, root, origin)
		u.Info("Open %s and enter the setup token to create your admin account.", origin)
		u.Success(in.success(origin, cert, []ui.KV{{Key: "Setup token", Value: token}}))
		return nil
	}
	adminID, err := runSetup(ctx, in.app, client, inst, &in.admin, in.answers)
	if err != nil {
		return err
	}
	in.adminID = adminID
	signInAfterSetup(ctx, in.app, client, adminID)
	if in.answers != nil {
		firstInvite(ctx, in.app, client, *in.answers)
	}
	rootCertCard(u, root, origin)
	u.Success(in.success(origin, cert, []ui.KV{{Key: "Admin", Value: in.admin.summary()}}))
	return nil
}

func (in *installer) finishStopped(token string) error {
	u := in.app.UI
	inst := in.inst
	inst.State.Stopped = true
	inst.State.Installing = false
	if err := inst.Save(); err != nil {
		return ui.Wrap(err, fmt.Sprintf("Couldn't save your settings in %s.", inst.Dir), "Check that you can write to it, then run rexctl install again.")
	}
	u.OK("Saved Fluxer and its settings in %s.", inst.Dir)
	if in.f.SetupWhere != "terminal" {
		u.Info("Run %s, then open %s and enter this setup token to create your admin account.", in.asRun("rexctl start"), inst.Origin())
		u.Highlight(token)
		return nil
	}
	u.Next(in.asRun("rexctl start"), in.asRun("rexctl setup"))
	return nil
}

func (in *installer) asRun(cmd string) string {
	if in.native || ranWithSudo() {
		return "sudo " + cmd
	}
	return cmd
}

func (in *installer) success(origin, cert string, extra []ui.KV) ui.Success {
	rows := append(extra, ui.KV{Key: "Folder", Value: in.inst.Dir}, ui.KV{Key: "Runtime", Value: in.runtimeLabel()})
	if cert != "" {
		rows = append(rows, ui.KV{Key: "Root certificate", Value: cert})
	}
	rows = append(rows, in.backupsRow()...)
	next := []string{"rexctl status"}
	if in.inst.State.SetupDone {
		next = append(next, "rexctl invites create")
	} else {
		next = append(next, "rexctl setup token")
	}
	if in.answers == nil && !usernameDomain(in.inst.Domain()) || in.answers != nil && in.answers.SignIn == setup.SignInEmail && in.answers.SMTP == nil {
		next = append(next, "rexctl email set")
	}
	for i := range next {
		next[i] = in.asRun(next[i])
	}
	title := "Your instance is live."
	if in.noCert {
		title = "Your instance is running. Browsers show a certificate warning until its certificate arrives."
	}
	return ui.Success{Title: title, URL: origin, Rows: rows, Next: next, Elapsed: time.Since(in.started), Celebrate: true, QR: origin}
}
