// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"errors"
	"fmt"
	"os"
	"runtime"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
)

func init() {
	Register(Check{ID: "engine", Phases: Preinstall | Installed | Voice, Run: runEngine, Fix: fixEngine, SafeFix: true})
}

func runEngine(ctx context.Context, e *Env) Result {
	name := e.engineName()
	if e.Engine == nil {
		switch engine.ClassOf(e.engineErr) {
		case engine.ClassDenied:
			if name == "Podman" {
				return fail("This account isn't allowed to use Podman.", "Run rexctl as the account that runs Podman, then run rexctl doctor.")
			}
			if runtime.GOOS == "windows" {
				return fail("This account isn't allowed to use Docker.", "Add your account to docker-users, then sign out and back in and run rexctl doctor.")
			}
			return fail("This account isn't allowed to use Docker.", "Add your account to the docker group with sudo usermod -aG docker $USER. Sign out and back in, then run rexctl doctor.")
		case engine.ClassWindowsContainers:
			return fail("Docker Desktop runs Windows containers, and Fluxer needs Linux containers.", "Switch Docker Desktop to Linux containers, then run rexctl doctor.")
		case engine.ClassPodmanTooOld:
			return fail("This Podman is too old for Fluxer.", fmt.Sprintf("Update Podman to %s or newer, then run rexctl doctor.", engine.MinPodman))
		case engine.ClassEngineChanged:
			var ee *engine.Error
			if errors.As(e.engineErr, &ee) && ee.Replaced {
				return fail("The engine Fluxer uses was replaced, and the new one doesn't have this instance's data.",
					"Start the engine Fluxer was installed on. If it was reset or reinstalled, restore a backup with rexctl restore <file>.")
			}
			return fail("The container engine on this machine isn't the one your instance runs on.", "Start the engine you installed Fluxer with, then run rexctl doctor.")
		}
		if e.podmanSocketOff() {
			return podmanSocketResult(ctx, e)
		}
		switch {
		case !e.installed() && runtime.GOOS == "linux" && name == "Docker":
			return fail("Docker isn't installed or isn't running.", "Run rexctl install and let it install Docker.")
		case name == "Docker" && runtime.GOOS != "linux":
			return fail("Docker Desktop isn't running.", "Open Docker Desktop and wait until its engine is running, then run rexctl doctor.")
		}
		return fail(name+" isn't running.", "Start "+name+", then run rexctl doctor.")
	}
	minimum := e.Engine.MinVersion()
	if e.Engine.TooOld || !engine.AtLeast(e.Engine.Version, minimum) {
		if e.Engine.Kind == engine.Podman && e.installed() {
			return fail(fmt.Sprintf("Podman %s is too old for Fluxer.", e.Engine.Version),
				fmt.Sprintf("Update Podman to %s or newer, or move to Docker with rexctl backup and then rexctl restore <file> --engine docker.", minimum))
		}
		return fail(fmt.Sprintf("%s %s is too old for Fluxer.", name, e.Engine.Version),
			fmt.Sprintf("Update %s to %s or newer, then run rexctl doctor.", name, minimum))
	}
	return pass(fmt.Sprintf("%s %s is running.", name, e.Engine.Version))
}

func podmanSocketResult(ctx context.Context, e *Env) Result {
	s := host.CheckPodman(ctx, os.Geteuid() != 0, true)
	if !s.Systemd {
		return fail("Podman's API socket isn't running.", "Start podman.socket, then run rexctl doctor.")
	}
	fix := "Run rexctl doctor --fix or rexctl install to start it."
	if e.installed() {
		fix = "Run rexctl doctor --fix to start it."
	}
	r := fixable(fail("Podman's API socket isn't running.", fix))
	if cmds := s.ManualFix(); len(cmds) > 0 {
		r.Manual = "Run " + strings.Join(cmds, " and ") + ", then run rexctl doctor."
	}
	return r
}

func fixEngine(ctx context.Context, e *Env) error {
	if !e.podmanSocketOff() {
		return nil
	}
	if err := host.FixPodman(ctx, host.CheckPodman(ctx, os.Geteuid() != 0, true)); err != nil {
		return err
	}
	e.reconnect(ctx)
	return nil
}
