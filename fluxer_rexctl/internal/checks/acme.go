// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"bytes"
	"context"
	"fmt"
	"net/url"
	"regexp"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
)

type acmeKind int

const (
	acmePending acmeKind = iota
	acmeUnknown
	acmeDNS
	acmeCAA
	acmeRateLimited
	acmeRefused
	acmeOtherServer
	acmeTimeout
	acmeSlow
	acmeUnreachable
)

type acmeLimit int

const (
	limitOther acmeLimit = iota
	limitExactSet
	limitDomain
	limitFailedTries
	limitThisMachine
)

type acmeProblem struct {
	kind  acmeKind
	limit acmeLimit
	name  string
	until time.Time
}

var (
	acmeType       = regexp.MustCompile(`urn:ietf:params:acme:error:([A-Za-z]+)`)
	acmeRetryAfter = regexp.MustCompile(`retry after (\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}) UTC`)
	acmeQuotedName = regexp.MustCompile(`for \\?"([^"\\]+)\\?"`)
	acmeNetErr     = `(?:i/o timeout|connection refused|no such host|network is unreachable|no route to host|TLS handshake timeout|context deadline exceeded)`
	letsEncrypt    = acmeCA{name: "Let's Encrypt", host: `acme[-\w.]*\.api\.letsencrypt\.org`, caa: "letsencrypt.org"}
)

type acmeCA struct {
	name string
	host string
	caa  string
}

func (c acmeCA) title() string {
	return strings.ToUpper(c.name[:1]) + c.name[1:]
}

func (c acmeCA) unreachable() *regexp.Regexp {
	return regexp.MustCompile(`lookup ` + c.host + `\b|"https://` + c.host + `[^"\\]*\\?":[^"]*?` + acmeNetErr)
}

func (e *Env) acmeCA() acmeCA {
	dir := e.value("FLUXER_EDGE_ACME_CA")
	if dir == "" {
		return letsEncrypt
	}
	u, err := url.Parse(dir)
	if err != nil || u.Hostname() == "" || regexp.MustCompile(`^`+letsEncrypt.host+`$`).MatchString(u.Hostname()) {
		return letsEncrypt
	}
	return acmeCA{name: "the certificate authority", host: regexp.QuoteMeta(u.Hostname()), caa: "your certificate authority"}
}

func classifyACME(logs string, ca acmeCA) acmeProblem {
	unreachable := ca.unreachable()
	lines := strings.Split(logs, "\n")
	for i := len(lines) - 1; i >= 0; i-- {
		if p, ok := classifyACMELine(lines[i], unreachable); ok {
			return p
		}
	}
	return acmeProblem{kind: acmePending}
}

func classifyACMELine(line string, unreachable *regexp.Regexp) (acmeProblem, bool) {
	var kind string
	for _, m := range acmeType.FindAllStringSubmatch(line, -1) {
		if m[1] != "compound" {
			kind = m[1]
		}
	}
	if kind == "" {
		if unreachable.MatchString(line) {
			return acmeProblem{kind: acmeUnreachable}, true
		}
		return acmeProblem{}, false
	}
	switch kind {
	case "caa":
		return acmeProblem{kind: acmeCAA}, true
	case "dns":
		return acmeProblem{kind: acmeDNS}, true
	case "rateLimited":
		return rateLimit(line), true
	case "connection":
		switch {
		case strings.Contains(line, "Timeout after connect"):
			return acmeProblem{kind: acmeSlow}, true
		case strings.Contains(line, "Timeout"):
			return acmeProblem{kind: acmeTimeout}, true
		}
		return acmeProblem{kind: acmeRefused}, true
	case "unauthorized", "incorrectResponse", "tls":
		return acmeProblem{kind: acmeOtherServer}, true
	}
	return acmeProblem{kind: acmeUnknown}, true
}

func rateLimit(line string) acmeProblem {
	p := acmeProblem{kind: acmeRateLimited}
	if m := acmeRetryAfter.FindStringSubmatch(line); m != nil {
		if t, err := time.Parse(time.DateTime, m[1]); err == nil {
			p.until = t
		}
	}
	if m := acmeQuotedName.FindStringSubmatch(line); m != nil {
		p.name = m[1]
	}
	switch {
	case strings.Contains(line, "this exact set of identifiers"):
		p.limit = limitExactSet
	case strings.Contains(line, "too many failed authorizations"):
		p.limit = limitFailedTries
	case strings.Contains(line, "too many certificates") && p.name != "":
		p.limit = limitDomain
	case strings.Contains(line, "too many new orders"), strings.Contains(line, "too many new registrations"):
		p.limit = limitThisMachine
	}
	return p
}

func (p acmeProblem) fix(domain string, ca acmeCA) string {
	ports := FirewallFix + ", then run rexctl doctor."
	switch p.kind {
	case acmeDNS:
		return fmt.Sprintf("%s couldn't look up %s in DNS. A new DNS record can take a while to show up everywhere and Fluxer keeps trying by itself. If it still fails later, check the record at your DNS provider, then run rexctl doctor.", ca.title(), domain)
	case acmeCAA:
		return fmt.Sprintf("A CAA record on %s or its parent domain doesn't allow %s. At your DNS provider, add a CAA record that allows %s, or delete the CAA records, then run rexctl doctor.", domain, ca.name, ca.caa)
	case acmeRateLimited:
		return p.rateLimitFix(domain, ca)
	case acmeRefused:
		return fmt.Sprintf("%s couldn't connect to this machine on port 80 or 443. %s", ca.title(), ports)
	case acmeTimeout:
		return fmt.Sprintf("%s timed out connecting to this machine on port 80 or 443. %s", ca.title(), ports)
	case acmeSlow:
		return fmt.Sprintf("%s connected to this machine on port 80 or 443 but got no answer in time. Run rexctl logs edge to see whether the edge is overloaded.", ca.title())
	case acmeOtherServer:
		return fmt.Sprintf("%s got a reply from a different web server on port 80 or 443. Check that your router or hosting provider forwards both ports to this machine, then run rexctl doctor.", ca.title())
	case acmeUnreachable:
		return fmt.Sprintf("This machine can't connect to %s. Check that its DNS lookups and outgoing HTTPS connections work, then run rexctl doctor.", ca.name)
	case acmeUnknown:
		return fmt.Sprintf("%s returned an error. Run rexctl logs edge to see what it said.", ca.title())
	}
	return fmt.Sprintf("Fluxer hasn't got one from %s yet and keeps trying. Run rexctl logs edge to see why.", ca.name)
}

func (p acmeProblem) rateLimitFix(domain string, ca acmeCA) string {
	var why string
	switch p.limit {
	case limitExactSet:
		why = fmt.Sprintf("%s has issued as many certificates for this name as it allows in a week.", ca.title())
	case limitDomain:
		why = fmt.Sprintf("%s has issued as many certificates for %s and its subdomains as it allows in a week.", ca.title(), p.name)
	case limitFailedTries:
		why = fmt.Sprintf("%s paused new tries for %s after too many failed ones.", ca.title(), domain)
	case limitThisMachine:
		why = fmt.Sprintf("%s has had too many requests from this machine for now.", ca.title())
	default:
		why = fmt.Sprintf("%s is limiting requests for this name for now.", ca.title())
	}
	again := " Fluxer tries again by itself."
	if !p.until.IsZero() {
		again = fmt.Sprintf(" Fluxer tries again by itself after %s.", p.until.Local().Format("15:04 MST on 2 January"))
	}
	if p.limit == limitFailedTries {
		return why + again + " Run rexctl logs edge to see what failed."
	}
	return why + again
}

func (e *Env) edgeLogs(ctx context.Context) string {
	var buf bytes.Buffer
	opts := engine.LogsOptions{Tail: 5000, Stdout: &buf}
	var err error
	switch {
	case e.Native:
		err = e.stack().Logs(ctx, opts, "edge")
	case e.Project != nil:
		err = e.Project.Logs(ctx, opts, "edge")
	default:
		return ""
	}
	if err != nil {
		return ""
	}
	return tlsLines(buf.String())
}

func tlsLines(logs string) string {
	var out strings.Builder
	for line := range strings.Lines(logs) {
		if strings.Contains(line, `"logger":"tls`) {
			out.WriteString(line)
		}
	}
	return out.String()
}
