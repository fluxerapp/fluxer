// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/backup"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

func openStateLog(inst *instance.Instance, name string, flag int) (*os.File, error) {
	root, err := os.OpenRoot(inst.Dir)
	if err != nil {
		return nil, err
	}
	defer root.Close()
	return root.OpenFile(filepath.Join(instance.StateDirName, name), flag, 0o600)
}

func writeFailureLog(ctx context.Context, inst *instance.Instance, p *engine.Project, name string, cause error, waiting []string) string {
	ctx = context.WithoutCancel(ctx)
	waiting = slices.Clone(waiting)
	for _, svc := range notRunning(ctx, p) {
		if !slices.Contains(waiting, svc) {
			waiting = append(waiting, svc)
		}
	}
	path := inst.Path(instance.StateDirName, name)
	f, err := openStateLog(inst, name, os.O_CREATE|os.O_WRONLY|os.O_TRUNC)
	if err != nil {
		return ""
	}
	defer f.Close()
	fmt.Fprintf(f, "rexctl: %v\n", cause)
	if len(waiting) > 0 {
		fmt.Fprintf(f, "rexctl: not ready: %s\n", strings.Join(waiting, ", "))
	}
	fmt.Fprintln(f)
	_ = p.Logs(ctx, engine.LogsOptions{Tail: 200, Stdout: f}, waiting...)
	return path
}

func writeBackupLog(inst *instance.Instance, name string, cause error) string {
	text := fmt.Sprintf("rexctl: backup failed: %v\n", cause)
	var ee *ui.ExitError
	if errors.As(cause, &ee) && ee.Err != nil && ee.Msg != "" {
		text += fmt.Sprintf("rexctl: %v\n", ee.Err)
	}
	var dump *backup.DumpError
	if errors.As(cause, &dump) && dump.Output != "" {
		text += "\n" + dump.Output + "\n"
	}
	f, err := openStateLog(inst, name, os.O_CREATE|os.O_WRONLY|os.O_TRUNC)
	if err != nil {
		return ""
	}
	defer f.Close()
	if _, err := f.WriteString(text); err != nil {
		return ""
	}
	return inst.Path(instance.StateDirName, name)
}

func notRunning(ctx context.Context, p *engine.Project) []string {
	containers, err := p.PS(ctx)
	if err != nil {
		return nil
	}
	var out []string
	for _, c := range containers {
		ok := c.State == "running" && c.Health != "unhealthy" && c.RestartCount == 0 || stack.OneShot[c.Service] && c.State == "exited" && c.ExitCode == 0
		if !ok && !slices.Contains(out, c.Service) {
			out = append(out, c.Service)
		}
	}
	return out
}
