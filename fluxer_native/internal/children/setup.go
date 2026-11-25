// SPDX-License-Identifier: AGPL-3.0-or-later

package children

import (
	"regexp"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/updates"
)

const (
	methodPodman      = "podman"
	setupTokenCommand = "fluxerd setup-token"
)

var helmNamespace = regexp.MustCompile(`\s-n\s+(\S+)`)

func setupMethod(vars spec.Env) string {
	m := updates.DetectMethod(vars[spec.InstallMethodEnv], vars[spec.ContainerEnv], vars["INVOCATION_ID"])
	if vars["container"] == methodPodman && (m == updates.MethodDocker || m == updates.MethodContainer) {
		return methodPodman
	}
	return m
}

func RexctlCommand(vars spec.Env, args string) string {
	if before, after, ok := strings.Cut(strings.TrimSpace(vars[spec.UpgradeCommandEnv]), "rexctl upgrade"); ok {
		return before + "rexctl " + args + after
	}
	return "rexctl " + args
}

func setupCommand(method string, vars spec.Env) string {
	engine := "docker"
	if vars["container"] == methodPodman {
		engine = methodPodman
	}
	switch method {
	case updates.MethodRexctl:
		return RexctlCommand(vars, "setup token")
	case updates.MethodSystemd:
		return "sudo -u fluxer /opt/fluxer/current/bin/" + setupTokenCommand
	case updates.MethodHelm:
		ns := "fluxer"
		if m := helmNamespace.FindStringSubmatch(vars[spec.UpgradeCommandEnv]); m != nil {
			ns = m[1]
		}
		pod := strings.TrimSpace(vars["HOSTNAME"])
		if pod == "" {
			pod = "fluxer-0"
		}
		return "kubectl -n " + ns + " exec " + pod + " -- " + setupTokenCommand
	case updates.MethodCompose:
		return engine + " compose exec fluxer " + setupTokenCommand
	case updates.MethodDocker, methodPodman:
		return engine + " exec fluxer " + setupTokenCommand
	case updates.MethodContainer:
		return setupTokenCommand
	}
	return ""
}

func SetupCommand(vars spec.Env) string {
	if method := setupMethod(vars); method != "" {
		return setupCommand(method, vars)
	}
	return ""
}
