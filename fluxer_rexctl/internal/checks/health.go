// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"context"
	"crypto/tls"
	"net"
	"net/http"
	"slices"
	"strings"
	"time"
)

func init() {
	Register(Check{ID: "health", Phases: Installed, Run: runHealth})
}

func (e *Env) localAddr(key, def string) string {
	v := e.value(key)
	if v == "" {
		v = def
	}
	return localHostPort(v)
}

func localHostPort(v string) string {
	host, port, err := net.SplitHostPort(v)
	if err != nil {
		host, port = "", strings.TrimPrefix(v, ":")
	}
	if host == "" || host == "0.0.0.0" || host == "::" || host == "[::]" {
		host = "127.0.0.1"
	}
	return net.JoinHostPort(host, port)
}

func (e *Env) healthURLs() []string {
	switch e.mode() {
	case modeProxy:
		return []string{"http://" + localHostPort(e.Bind) + "/_health"}
	case modeTunnel:
		return []string{"http://" + e.localAddr("FLUXER_HTTP_PORT", "127.0.0.1:80") + "/_health"}
	}
	if strings.EqualFold(e.value("FLUXER_PUBLIC_SCHEME"), "http") {
		return []string{"http://" + e.localAddr("FLUXER_HTTP_PORT", "80") + "/_health"}
	}
	return []string{
		"https://" + e.localAddr("FLUXER_HTTPS_PORT", "443") + "/_health",
		"http://" + e.localAddr("FLUXER_HTTP_PORT", "80") + "/_health",
	}
}

func (e *Env) edgePublished() bool {
	if e.Project == nil || e.Project.Model == nil {
		return true
	}
	svc, ok := e.Project.Model.Services["edge"]
	return !ok || len(svc.Ports) > 0
}

func (e *Env) localClient() *http.Client {
	return &http.Client{
		Timeout: 10 * time.Second,
		Transport: &http.Transport{
			Proxy:           nil,
			TLSClientConfig: &tls.Config{ServerName: e.Domain, InsecureSkipVerify: true},
		},
		CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse },
	}
}

func runHealth(ctx context.Context, e *Env) Result {
	if e.Instance == nil {
		return skip("")
	}
	states, err := e.serviceStates(ctx)
	if err != nil || !slices.ContainsFunc(states, func(s ServiceState) bool { return s.Name == "edge" && s.Running() }) {
		return skip("")
	}
	if !e.edgePublished() {
		if !answersInsideEdge(ctx, e) {
			return fail("Fluxer doesn't answer inside the edge container.", "Run rexctl logs edge to see why.")
		}
		return pass("Fluxer answers inside the edge container.")
	}
	client := e.localClient()
	answered := false
	for _, u := range e.healthURLs() {
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, u, nil)
		if err != nil {
			continue
		}
		req.Host = e.Domain
		resp, err := client.Do(req)
		if err != nil {
			continue
		}
		resp.Body.Close()
		if resp.StatusCode == http.StatusOK {
			return pass("Fluxer answers on this machine.")
		}
		if strings.HasPrefix(u, "https://") {
			answered = true
			continue
		}
		if !answered && resp.StatusCode >= 300 && resp.StatusCode < 400 {
			return pass("Fluxer answers on this machine.")
		}
	}
	if answered {
		return fail("Fluxer's web server answers with an error.", "Run rexctl logs api to see why.")
	}
	if answersInsideEdge(ctx, e) {
		return warn("Fluxer answers inside the edge container, but not on this machine's ports 80 and 443.", "That's fine if your own reverse proxy forwards to the edge container. Run rexctl proxy on to tell rexctl about it.")
	}
	return fail("Fluxer doesn't answer on this machine.", "Run rexctl logs edge to see why.")
}

func answersInsideEdge(ctx context.Context, e *Env) bool {
	probe := "http://127.0.0.1:8088/_edge"
	if e.mode() == modeProxy {
		probe = "http://127.0.0.1:8080/_health"
	}
	_, err := e.exec(ctx, "edge", "", "wget", "-q", "-O", "/dev/null", probe)
	return err == nil
}
