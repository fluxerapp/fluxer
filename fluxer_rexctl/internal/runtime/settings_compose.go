// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"io"
	"slices"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

type composeSettings struct{ c *compose }

func (composeSettings) Supports(key string) (bool, string) { return true, "" }

func (composeSettings) Validate(ctx context.Context, p *deploy.Prepared) error { return nil }

func (s composeSettings) Preview(ctx context.Context, p *deploy.Prepared) (Preview, error) {
	if p == nil || p.Unchanged {
		return Preview{}, nil
	}
	if p.Engine {
		return Preview{Notes: p.Preview.Notes()}, nil
	}
	e, err := s.c.engine(ctx)
	if err != nil {
		return Preview{}, err
	}
	dp, err := deploy.Plan(ctx, s.c.inst, e, p)
	if err != nil {
		return Preview{}, s.c.engineError(err, "rexctl can't work out what the change restarts.")
	}
	return previewFrom(dp), nil
}

func (s composeSettings) Apply(ctx context.Context, step *ui.Step, p *deploy.Prepared) (*deploy.Change, error) {
	e, err := s.c.engine(ctx)
	if err != nil {
		return nil, err
	}
	inst := s.c.inst
	var logPath string
	opts := deploy.ApplyOptions{
		Wait: true,
		Failed: func(proj *engine.Project, cause error) {
			logPath = writeStartLog(inst, cause, func(w io.Writer) error {
				return proj.Logs(context.WithoutCancel(ctx), engine.LogsOptions{Tail: startLogLines, Stdout: w})
			})
		},
		OnEvent: func(ev engine.Event) {
			if ev.Text != "" && step != nil {
				step.Log(ev.ID + " " + ev.Text)
			}
		},
	}
	change, err := deploy.SetPrepared(ctx, inst, e, p, opts)
	if err != nil {
		return change, ApplyFailure(change, err, logPath)
	}
	return change, nil
}

func (s composeSettings) Render(ctx context.Context, w io.Writer) error {
	inst := s.c.inst
	sp, err := deploy.Spec(inst)
	if err != nil {
		return err
	}
	for name, value := range sp.Env {
		_, generated := inst.State.Secrets[name]
		if generated || settings.SecretName(name) {
			sp.Env[name] = settings.Masked
		} else {
			sp.Env[name] = settings.Mask(settings.Key{Name: name}, value)
		}
	}
	proj, err := engine.Load(ctx, sp)
	if err != nil {
		return ui.Wrap(err, "rexctl can't read Fluxer's files.", "Run rexctl doctor to see what's wrong.")
	}
	out, err := proj.Model.MarshalYAML()
	if err != nil {
		return err
	}
	_, err = w.Write(out)
	return err
}

func previewFrom(dp deploy.Preview) Preview {
	pv := Preview{Notes: dp.Notes()}
	if len(dp.Networks) > 0 {
		pv.Restarts = []string{"all"}
		return pv
	}
	for _, svc := range append(slices.Clone(dp.Restarts), dp.Removes...) {
		if part := PartOf(svc); !slices.Contains(pv.Restarts, part) {
			pv.Restarts = append(pv.Restarts, part)
		}
	}
	return pv
}

func ApplyFailure(change *deploy.Change, err error, logPath string) error {
	var ee *ui.ExitError
	switch {
	case errors.Is(err, context.Canceled) && (change == nil || change.Reverted):
		return err
	case change != nil && change.Reverted:
		fix := "Run rexctl logs to see why."
		if logPath != "" {
			fix = "Read " + logPath + " to see why."
		}
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "Fluxer didn't start with the new settings. rexctl put the old ones back.", Fix: fix, Err: err, LogPath: logPath}
	case change != nil:
		return &ui.ExitError{Code: ui.ExitFailed, Msg: "Fluxer didn't start with the new settings, and rexctl couldn't put the old ones back.", Fix: "Run rexctl doctor to see what's wrong.", Err: err, LogPath: logPath}
	case errors.As(err, &ee):
		return err
	}
	return ui.Wrap(err, fmt.Sprintf("Couldn't apply %s.", ui.JoinAnd(changedKeys(change))), "Run rexctl doctor to see what's wrong.")
}

func changedKeys(change *deploy.Change) []string {
	if change == nil || len(change.Keys) == 0 {
		return []string{"the new settings"}
	}
	return change.Keys
}
