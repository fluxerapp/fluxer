// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"fmt"
	"slices"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/voice"
)

const voiceEdgeName = "edge"

func voiceMachineRefusal(dir string, inst *instance.Instance) error {
	return ui.Refused(fmt.Sprintf("%s holds voice servers that joined %s, not a Fluxer instance.", dir, voiceInstances(inst)), "On a voice machine, use rexctl voice status, rexctl logs, rexctl restart, rexctl doctor or rexctl upgrade.")
}

func ownVoiceEdge(inst *instance.Instance) bool {
	return fluxerdVoice(inst) || slices.ContainsFunc(inst.State.Voice, func(s instance.VoiceServer) bool { return s.Edge == voice.EdgeOwn })
}

func voiceEdgeProject(ctx context.Context, app *App, inst *instance.Instance) (*engine.Project, error) {
	e, err := app.Engine(ctx, inst)
	if err != nil {
		return nil, err
	}
	return e.Named(voice.EdgeProject, inst.Path(voice.EdgeDir)), nil
}

func voiceMachineLogs(ctx context.Context, app *App, inst *instance.Instance, args []string, o rt.LogOptions) error {
	edge := ownVoiceEdge(inst)
	if len(args) == 1 && args[0] == voiceEdgeName {
		if !edge {
			return ui.Refused("The voice servers on this machine don't run an edge of their own.", "Run rexctl logs edge in the folder of the Fluxer instance they use.")
		}
		return voiceEdgeLogs(ctx, app, inst, o)
	}
	if o.Follow && len(args) == 0 {
		_, servers, err := voiceServersHere(inst, nil)
		if err != nil {
			return err
		}
		if len(servers) > 1 || edge {
			example := "rexctl logs <id> --follow"
			if len(servers) == 1 {
				example = "rexctl logs " + servers[0].ID + " --follow"
			}
			if edge {
				example += " or rexctl logs edge --follow"
			}
			return ui.Usage("Follow one thing at a time, for example " + example + ".")
		}
	}
	if err := voiceLogs(ctx, app, inst, args, o); err != nil {
		return err
	}
	if len(args) == 1 || !edge {
		return nil
	}
	app.UI.Section(voiceEdgeName)
	return voiceEdgeLogs(ctx, app, inst, o)
}

func voiceEdgeLogs(ctx context.Context, app *App, inst *instance.Instance, o rt.LogOptions) error {
	if fluxerdVoice(inst) {
		return voiceHostLogs(ctx, inst, app.UI.Stdout(), o)
	}
	p, err := voiceEdgeProject(ctx, app, inst)
	if err != nil {
		return err
	}
	opts := engine.LogsOptions{Follow: o.Follow, Since: o.Since, Tail: o.Tail, Stdout: app.UI.Stdout()}
	if err := p.Logs(ctx, opts, voiceEdgeName); err != nil && ctx.Err() == nil {
		return ui.Wrap(rt.EngineError("", err), "Couldn't read the logs of the edge.", "Run rexctl voice status to check the voice servers.")
	}
	return nil
}

func voiceMachineRestart(ctx context.Context, app *App, inst *instance.Instance) error {
	if err := voiceRestart(ctx, app, inst, nil, false); err != nil {
		return err
	}
	if !ownVoiceEdge(inst) || fluxerdVoice(inst) {
		return nil
	}
	p, err := voiceEdgeProject(ctx, app, inst)
	if err != nil {
		return err
	}
	return app.Locked(ctx, inst, func() error {
		return app.UI.Step(ctx, "Restarting the edge", func(ctx context.Context, s *ui.Step) error {
			if err := p.Restart(context.WithoutCancel(ctx), voiceEdgeName); err != nil {
				return ui.Wrap(rt.EngineError("", err), "Couldn't restart the edge.", "Run rexctl logs edge to see why.")
			}
			s.Done("Restarted the edge.")
			return nil
		})
	})
}

func voiceMachineBackup(app *App, inst *instance.Instance) error {
	app.UI.Result(fmt.Sprintf("This machine holds only voice servers of %s, so there's nothing to back up.", voiceInstances(inst)))
	app.UI.Info("If you lose it, run rexctl voice remove <id> --forget on the Fluxer machine, then rexctl voice join on a new machine.")
	return nil
}

func voiceMachineStatus(ctx context.Context, app *App, inst *instance.Instance) error {
	rows := voiceMachineRows(ctx, inst)
	v := ui.View{Title: "Voice servers for " + voiceInstances(inst), Tone: ui.ToneOK, Mood: ui.MoodIdle, Updated: time.Now()}
	v.Meta = []ui.KV{{Key: "Runs on", Value: voiceRuntimeLabel(inst)}, {Key: "Folder", Value: inst.Dir}}
	if fluxerdVoice(inst) && inst.State.Release != "" {
		v.Meta = append(v.Meta, ui.KV{Key: "Release", Value: inst.State.Release})
	}
	var down []string
	for _, r := range rows {
		state := ui.StateOK
		switch {
		case !r.up:
			state = ui.StateFail
			down = append(down, r.ID)
		case r.draining:
			state = ui.StateWarn
		}
		v.Parts = append(v.Parts, ui.PartRow{Name: r.ID, Members: r.Endpoint, State: state})
	}
	fix := ""
	switch {
	case len(rows) == 0:
		v.Headline, v.Tone, v.Mood = "This machine runs no voice servers.", ui.ToneWarn, ui.MoodWorried
		fix = "Run rexctl voice join to add one."
	case len(down) == 1:
		v.Headline, v.Tone, v.Mood = "Voice server "+down[0]+" isn't up.", ui.ToneFail, ui.MoodSad
		fix = "Run rexctl voice status to see why."
	case len(down) > 1:
		v.Headline, v.Tone, v.Mood = "Voice servers "+ui.JoinAnd(down)+" aren't up.", ui.ToneFail, ui.MoodSad
		fix = "Run rexctl voice status to see why."
	case len(rows) == 1:
		v.Headline = "The voice server is up."
	default:
		v.Headline = "Every voice server is up."
	}
	app.UI.StatusCard(v)
	if fix != "" {
		app.UI.Info("%s", fix)
	}
	if len(down) > 0 {
		app.SetExit(ui.ExitRefused)
	}
	return nil
}
