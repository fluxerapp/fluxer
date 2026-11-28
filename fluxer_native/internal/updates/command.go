// SPDX-License-Identifier: AGPL-3.0-or-later

package updates

import (
	"os"
	"slices"
	"strings"
)

const (
	MethodCompose = "compose"
	MethodDocker  = "docker"
	MethodHelm    = "helm"
	MethodSystemd = "systemd"
	MethodRexctl  = "rexctl"

	MethodContainer = "container"

	VersionMark = "{version}"
	Image       = "ghcr.io/fluxerapp/fluxer"
	DocsURL     = "https://docs.fluxer.com/operator/upgrades/"
	ReleasesURL = "https://github.com/fluxerapp/fluxer/releases"
	manualURL   = "https://docs.fluxer.com/operator/linux-manual/#upgrade"
)

var Methods = []string{MethodCompose, MethodDocker, MethodHelm, MethodSystemd, MethodRexctl}

func KnownMethod(m string) bool { return slices.Contains(Methods, m) }

func DetectMethod(set, container, invocationID string) string {
	if m := strings.ToLower(strings.TrimSpace(set)); KnownMethod(m) {
		return m
	}
	if container == "1" {
		return MethodContainer
	}
	if invocationID != "" {
		return MethodSystemd
	}
	return ""
}

const podmanMarker = "/run/.containerenv"

func InPodman() bool {
	_, err := os.Stat(podmanMarker)
	return err == nil
}

func ComposeCommand(version string) string { return composeCommand(version, "docker compose") }

func PodmanComposeCommand(version string) string { return composeCommand(version, "podman compose") }

func composeCommand(version, tool string) string {
	image := Image + ":" + version
	return `sed -i.bak -E 's#ghcr\.io/fluxerapp/fluxer:(\$\{[^}]*\}|[^[:space:]"'\'']*)#` + image + `#' compose.yaml && grep -q '` + image + `' compose.yaml && ` + tool + ` pull && ` + tool + ` up -d`
}

func Command(method, override, version, goarch string, podman bool) (command, then string) {
	if override = strings.TrimSpace(override); override != "" {
		return strings.ReplaceAll(override, VersionMark, version), ""
	}
	image := Image + ":" + version
	switch method {
	case MethodCompose:
		if podman {
			return PodmanComposeCommand(version), "Run it in the folder with compose.yaml."
		}
		return ComposeCommand(version), "Run it in the folder with compose.yaml."
	case MethodDocker:
		return "docker pull " + image + " && docker stop -t 130 fluxer && docker rm fluxer",
			"Then run your docker run line again with " + image + "."
	case MethodSystemd:
		return "version=" + version + " && curl -fLO https://github.com/fluxerapp/fluxer/releases/download/fluxer-release%40$version/fluxer-$version-linux-" + goarch + ".tar.zst",
			"Then follow the upgrade steps at " + manualURL + "."
	case MethodContainer:
		return "", "Change the image to " + image + " where you set Fluxer up, such as compose.yaml, your docker run line or your container panel, and start the container again from it. See " + DocsURL + " for the steps for each."
	}
	return "", "See " + DocsURL + " for the steps."
}
