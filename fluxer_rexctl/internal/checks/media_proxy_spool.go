// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"io"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

const (
	mediaProxySpoolDir    = `d="${FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SPOOL_DIR:-${TMPDIR:-/tmp}}"`
	mediaProxySpoolProbe  = mediaProxySpoolDir + `; f="$(mktemp -p "$d")" && rm -f "$f"`
	mediaProxySpoolRepair = mediaProxySpoolDir + `; mkdir -p "$d" && chmod 1777 "$d"`
)

func init() {
	Register(Check{ID: "media-proxy-spool", Phases: Installed, Run: runMediaProxySpool, Fix: fixMediaProxySpool, SafeFix: true})
}

func runMediaProxySpool(ctx context.Context, e *Env) Result {
	if e.Instance == nil || !e.running(ctx, "media-proxy") {
		return skip("")
	}
	out, err := e.exec(ctx, "media-proxy", "", "sh", "-c", mediaProxySpoolProbe)
	if err == nil {
		return pass("The media proxy can write its temporary files.")
	}
	r := fixable(fail("The media proxy can't write its temporary files, so uploads from the desktop app fail.", "Run rexctl doctor --fix to make its temporary folder writable."))
	r.Details = []string{out}
	return r
}

func fixMediaProxySpool(ctx context.Context, e *Env) error {
	if e.Project == nil {
		return errNoProject
	}
	return e.Project.Exec(ctx, "media-proxy", engine.ExecOptions{User: "0", Stdout: io.Discard, Stderr: io.Discard}, "sh", "-c", mediaProxySpoolRepair)
}
