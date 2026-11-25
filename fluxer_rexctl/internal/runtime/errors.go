// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path"
	goruntime "runtime"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/host"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/release"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

var ErrNeedsRoot = errors.New("runtime: needs root")

func unsupported(msg, fix string) error {
	return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: fix, Err: ErrUnsupported}
}

func needsRoot(msg string) error {
	return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: "Run the command again with sudo.", Err: ErrNeedsRoot}
}

func NeedsContainers(command string) error {
	if command == "" {
		command = "This command"
	}
	return unsupported(command+" needs Docker or Podman, and this instance runs natively without containers.", "Run rexctl --help to see the commands for your instance.")
}

func NeedsCompose(command string) error {
	if command == "" {
		command = "This command"
	}
	return unsupported(command+" works only on the older Docker Compose layout, and this instance runs in one container.", "Run rexctl --help to see the commands for your instance.")
}

func NativeError(err error, msg string) error {
	var ee *ui.ExitError
	switch {
	case err == nil:
		return nil
	case errors.As(err, &ee), errors.Is(err, context.Canceled):
		return err
	case errors.Is(err, host.ErrStandaloneNative):
		return ui.Refused("fluxerd.service already runs Fluxer on this machine.", "Run systemctl disable --now fluxerd first, or keep using that install.")
	case errors.Is(err, native.ErrNoApt):
		return ui.Refused("Fluxer as a systemd service installs its packages with apt, and this machine has no apt.", "Use Debian or Ubuntu, or run rexctl install --runtime docker.")
	case errors.Is(err, native.ErrNotRoot):
		return needsRoot("A native instance is managed as root.")
	case errors.Is(err, native.ErrChecksum):
		return ui.Wrap(err, "The Fluxer download didn't match its checksum.", "Try again.")
	case errors.Is(err, release.ErrNoNativeBuild):
		return ui.Refused(err.Error(), "Try again after the next Fluxer release.")
	}
	var refused *native.RestoreRefusedError
	if errors.As(err, &refused) && refused.Reason != "" {
		return &ui.ExitError{Code: ui.ExitRefused, Msg: refused.Reason, Fix: "Nothing changed on this instance.", Err: err}
	}
	var space *native.SpaceError
	if errors.As(err, &space) {
		return ui.Wrap(err, "There isn't enough free space in "+space.Dir+" to "+space.What+".", "Free up "+ui.Bytes(space.Short())+" on that disk, then try again.")
	}
	var aptErr *native.AptError
	if errors.As(err, &aptErr) {
		return &ui.ExitError{Code: ui.ExitFailed, Msg: msg, Fix: "Fix what apt reports below, then run the command again.", Err: err, LogPath: aptErr.Log, Tail: aptErr.Tail}
	}
	return ui.Wrap(err, msg, "Run the command again with --verbose to see why.")
}

func EngineError(kind engine.Kind, err error) error {
	if err == nil {
		return nil
	}
	var ee *ui.ExitError
	if errors.As(err, &ee) || errors.Is(err, context.Canceled) {
		return err
	}
	refused := func(msg, fix string) error { return &ui.ExitError{Code: ui.ExitRefused, Msg: msg, Fix: fix, Err: err} }
	failed := func(msg, fix string) error { return &ui.ExitError{Code: ui.ExitFailed, Msg: msg, Fix: fix, Err: err} }
	desktop := kind != engine.Podman && goruntime.GOOS != "linux"
	name := kind.Name()
	var e *engine.Error
	errors.As(err, &e)
	switch engine.ClassOf(err) {
	case engine.ClassUnreachable:
		if desktop {
			return refused("Docker Desktop isn't running.", "Open Docker Desktop and wait until its engine is running, then try again.")
		}
		if fix := podmanSocketFix(kind); fix != "" {
			return refused("Podman is installed, but its API socket isn't running.", fix)
		}
		if kind == "" {
			return refused("Neither Docker nor Podman responds on this machine.", "Start Docker or Podman, then try again.")
		}
		return refused(name+" isn't running.", "Start "+name+", then try again.")
	case engine.ClassDenied:
		switch {
		case goruntime.GOOS == "windows":
			return refused("This account isn't allowed to use Docker.", "Add your account to docker-users, then sign out and back in.")
		case kind == engine.Podman:
			return refused("This account isn't allowed to use Podman.", "Run rexctl as the account that runs Podman.")
		}
		return refused("This account isn't allowed to use Docker.", "Add your account to the docker group with sudo usermod -aG docker $USER, then sign out and back in.")
	case engine.ClassWindowsContainers:
		return refused("Docker Desktop is set to Windows containers.", "Switch Docker Desktop to Linux containers, then try again.")
	case engine.ClassPodmanTooOld:
		return refused(fmt.Sprintf("Fluxer keeps running, but this needs Podman %s or newer.", strings.TrimSuffix(engine.MinPodman, ".0")),
			"Upgrade Podman, or move to Docker with rexctl backup and then rexctl restore <file> --engine docker.")
	case engine.ClassEngineChanged:
		other := "another engine"
		switch {
		case e == nil || e.Other == "":
		case e.Other == name || desktop && e.Other == "Docker Desktop":
			other = "a different " + e.Other + " engine"
		default:
			other = e.Other
		}
		if e != nil && e.Replaced {
			return refused(fmt.Sprintf("The engine Fluxer uses is now %s, which doesn't have this instance's data.", other),
				"Start the engine Fluxer was installed on. If it was reset or reinstalled, restore a backup with rexctl restore <file>.")
		}
		if desktop {
			return refused("Docker Desktop isn't running, and "+other+" responds in its place.", "Open Docker Desktop and wait until its engine is running, then try again.")
		}
		return refused(fmt.Sprintf("Fluxer runs on %s, but %s responds in its place.", name, other), "Start the engine Fluxer runs on, or point rexctl at it with rexctl config set engine.host.")
	case engine.ClassRateLimit:
		registry := "The image registry"
		if e == nil || len(e.Images) == 0 || dockerHub(e.Images) {
			registry = "Docker Hub"
		}
		msg := registry + " is limiting downloads from this machine."
		if e != nil && len(e.Images) > 0 {
			msg = fmt.Sprintf("%s is limiting downloads from this machine and %s didn't download. Images already on this machine weren't downloaded again.", registry, ui.JoinAnd(imageNames(e.Images)))
		}
		if registry == "Docker Hub" {
			return failed(msg, "Wait for Docker Hub's limit to reset, which takes up to 6 hours, then try again.")
		}
		return failed(msg, "Wait a while, then try again.")
	case engine.ClassCredentials:
		return failed("Docker's credential helper on this machine doesn't work.", "Remove credsStore from your Docker config.json, then try again.")
	case engine.ClassAuth:
		return failed("The image registry refused to send Fluxer's images.", "Sign in to the registry with docker login, or check images.registry with rexctl config get images.registry.")
	case engine.ClassImageNotFound:
		return failed("The image registry doesn't have an image Fluxer needs.", "Check the images.* settings with rexctl config list, then try again.")
	case engine.ClassDNS:
		return failed("This machine can't look up the image registry.", "Check this machine's DNS settings, then try again.")
	case engine.ClassTLS:
		return failed("rexctl couldn't make a secure connection to the image registry.", "Check this machine's clock and any proxy that inspects HTTPS, then try again.")
	case engine.ClassProxy:
		return failed("The proxy the engine uses refused the connection.", "Check the proxy settings of "+name+", then try again.")
	case engine.ClassTimeout:
		return failed("The connection timed out.", "Check this machine's internet connection, then try again.")
	case engine.ClassDiskFull:
		return failed("This machine ran out of disk space.", "Free up space, then try again.")
	case engine.ClassPortInUse:
		return failed("A port Fluxer needs is already in use on this machine.", "Run rexctl doctor to see which one.")
	case engine.ClassAddressPools:
		return failed("Docker has no free network addresses left for Fluxer.", "Remove the Docker networks you don't use with docker network prune, then try again.")
	case engine.ClassSubIDs:
		return failed("Your account has no subordinate user IDs, and Podman needs them to run Fluxer.", "Add your account to /etc/subuid and /etc/subgid, run podman system migrate, then try again.")
	case engine.ClassDependencyFailed:
		return failed("A Fluxer service didn't start, and the services that need it didn't start either.", "Run rexctl logs to see why.")
	case engine.ClassVolumeChanged:
		return refused("A data volume on this machine doesn't match what Fluxer expects, and rexctl left it alone.", "Run rexctl doctor to see what's wrong.")
	case engine.ClassInvalidConfig:
		return failed("rexctl can't load Fluxer's compose files.", "Run rexctl doctor to see what's wrong.")
	case engine.ClassForeignProject:
		return refused("Another Fluxer instance on this machine uses the same name.", "Run rexctl status --dir with that instance's folder to check it.")
	}
	return err
}

func dockerHub(refs []string) bool {
	for _, ref := range refs {
		first, _, found := strings.Cut(ref, "/")
		if !found || first == "docker.io" || (!strings.ContainsAny(first, ".:") && first != "localhost") {
			return true
		}
	}
	return false
}

func imageNames(refs []string) []string {
	var out []string
	for _, ref := range refs {
		name, _, _ := strings.Cut(ref, "@")
		name = path.Base(name)
		name, _, _ = strings.Cut(name, ":")
		if !slices.Contains(out, name) {
			out = append(out, name)
		}
	}
	return out
}

func podmanSocketFix(kind engine.Kind) string {
	if goruntime.GOOS != "linux" || (kind != "" && kind != engine.Podman) {
		return ""
	}
	if _, err := exec.LookPath("podman"); err != nil {
		return ""
	}
	if os.Geteuid() == 0 {
		return "Run systemctl enable --now podman.socket, then try again."
	}
	return "Run systemctl --user enable --now podman.socket, then try again."
}
