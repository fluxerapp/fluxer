// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"fmt"
	"net"
	"net/url"
	"strings"
)

const originVar = "FLUXER_PUBLIC_ORIGIN"

func init() {
	Register(Check{ID: "origin", Phases: Installed, Run: runOrigin, Fix: fixOrigin})
}

func (e *Env) derivedOrigin() string {
	scheme := strings.ToLower(e.value("FLUXER_PUBLIC_SCHEME"))
	if scheme == "" {
		scheme = "https"
	}
	port := e.value("FLUXER_PUBLIC_PORT")
	domain := strings.ToLower(e.Domain)
	if port == "" || (scheme == "https" && port == "443") || (scheme == "http" && port == "80") {
		return scheme + "://" + domain
	}
	return scheme + "://" + net.JoinHostPort(domain, port)
}

func (e *Env) composeOrigin() string {
	scheme := strings.ToLower(e.value("FLUXER_PUBLIC_SCHEME"))
	if scheme == "" {
		scheme = "https"
	}
	return scheme + "://" + strings.ToLower(e.Domain)
}

func normalOrigin(s string) string {
	u, err := url.Parse(strings.TrimSpace(s))
	if err != nil || u.Host == "" {
		return strings.TrimRight(strings.ToLower(strings.TrimSpace(s)), "/")
	}
	scheme := strings.ToLower(u.Scheme)
	host := strings.ToLower(u.Hostname())
	port := u.Port()
	if port == "" || (scheme == "https" && port == "443") || (scheme == "http" && port == "80") {
		return scheme + "://" + host
	}
	return scheme + "://" + net.JoinHostPort(host, port)
}

func runOrigin(ctx context.Context, e *Env) Result {
	if e.Instance == nil || e.Domain == "" {
		return skip("")
	}
	httpPort, httpsPort := e.value("FLUXER_HTTP_PORT"), e.value("FLUXER_HTTPS_PORT")
	if httpPort == "" {
		httpPort = "80"
	}
	if httpsPort == "" {
		httpsPort = "443"
	}
	if e.mode() == modeBundled && bindPort(httpPort) == bindPort(httpsPort) {
		return fail(fmt.Sprintf("Fluxer is set to use port %s for both HTTP and HTTPS.", httpsPort),
			"Pick another HTTP port with rexctl config set edge.http_port <port>.")
	}
	want := e.derivedOrigin()
	have := e.value("FLUXER_PUBLIC_ORIGIN")
	switch {
	case strings.Contains(have, "${"):
		return fixable(warn("Your instance's public address refers to another setting.",
			fmt.Sprintf("Run rexctl doctor --fix to write it out as %s.", want)))
	case have != "" && normalOrigin(have) != want:
		return fixable(fail(fmt.Sprintf("Your instance's public address %s doesn't match %s.", strings.TrimRight(have, "/"), want),
			fmt.Sprintf("Run rexctl doctor --fix to use %s everywhere.", want)))
	case have == "" && want != e.composeOrigin():
		return fixable(fail(fmt.Sprintf("Some Fluxer services leave out the port in %s.", want),
			fmt.Sprintf("Run rexctl doctor --fix to use %s everywhere.", want)))
	}
	return pass(fmt.Sprintf("Your instance's address is %s everywhere.", want))
}

func fixOrigin(ctx context.Context, e *Env) error {
	if e.state() == nil {
		return errNoProject
	}
	if _, raw := e.state().Settings[originVar]; raw {
		e.stage(originVar, nil)
		if normalOrigin(e.value(originVar)) == e.derivedOrigin() {
			return nil
		}
	}
	want := e.derivedOrigin()
	e.stage(originVar, &want)
	return nil
}
