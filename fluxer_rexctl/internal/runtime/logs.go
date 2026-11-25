// SPDX-License-Identifier: AGPL-3.0-or-later

package runtime

import (
	"bytes"
	"fmt"
	"io"
	"slices"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/deploy"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/stack"
)

type logPlan struct {
	services []string
	inRust   []string
	label    string
}

func planLogs(inst *instance.Instance, parts []string) (logPlan, error) {
	services, err := ServicesOf(parts)
	if err != nil {
		return logPlan{}, err
	}
	var lp logPlan
	if len(parts) == 1 {
		lp.label = strings.ToLower(strings.TrimSpace(parts[0]))
	}
	if len(services) == 0 {
		return lp, nil
	}
	if !deploy.VoiceBundled(inst) {
		services = slices.DeleteFunc(services, func(s string) bool { return s == "livekit" })
		if len(services) == 0 {
			return lp, unsupported(fmt.Sprintf("This instance runs without the bundled LiveKit, and %s has no logs here.", parts[0]), "Run rexctl voice status to check your voice servers.")
		}
	}
	lp.services, lp.inRust = stack.ModeServices(inst.State.Settings, services)
	if len(lp.services) == 0 {
		return lp, unsupported(fmt.Sprintf("This instance runs without %s, so it has no logs here.", parts[0]), "Run rexctl status to see what runs.")
	}
	return lp, nil
}

func (lp logPlan) line(service, msg string) (string, bool) {
	if service == stack.RustService {
		svc := stack.RustLogService(msg)
		if len(lp.inRust) > 0 && !slices.Contains(lp.inRust, svc) {
			return "", false
		}
		if svc != "" {
			service = svc
		}
	}
	part := lp.label
	if part == "" {
		part = PartOf(service)
	}
	return part + " | " + msg, true
}

type partWriter struct {
	w   io.Writer
	lp  logPlan
	buf []byte
}

func (p *partWriter) Write(b []byte) (int, error) {
	p.buf = append(p.buf, b...)
	for {
		i := bytes.IndexByte(p.buf, '\n')
		if i < 0 {
			return len(b), nil
		}
		raw := string(p.buf[:i])
		p.buf = p.buf[i+1:]
		service, msg, found := strings.Cut(raw, " | ")
		if !found {
			if _, err := io.WriteString(p.w, raw+"\n"); err != nil {
				return len(b), err
			}
			continue
		}
		out, keep := p.lp.line(service, msg)
		if !keep {
			continue
		}
		if _, err := io.WriteString(p.w, out+"\n"); err != nil {
			return len(b), err
		}
	}
}
