// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"cmp"
	"context"
	"crypto/tls"
	"io"
	"net"
	"net/http"
	"slices"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/settings"
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

func (e *Env) edgeAddr(key, def string) string {
	return settings.EdgeDialAddr(e.value(settings.EdgeListenVar), cmp.Or(e.value(key), def))
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
		return []string{"http://" + e.edgeAddr("FLUXER_HTTP_PORT", "80") + "/_health"}
	}
	return []string{
		"https://" + e.edgeAddr("FLUXER_HTTPS_PORT", "443") + "/_health",
		"http://" + e.edgeAddr("FLUXER_HTTP_PORT", "80") + "/_health",
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
			TLSClientConfig: &tls.Config{ServerName: settings.BareHost(e.Domain), InsecureSkipVerify: true},
		},
		CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse },
	}
}

func runHealth(ctx context.Context, e *Env) Result {
	if e.Instance == nil {
		return skip("")
	}
	if !e.edgeUp(ctx) {
		return skip("")
	}
	if !e.Native && !e.edgePublished() {
		if !answersInsideEdge(ctx, e) {
			return fail("Fluxer doesn't respond inside the edge container.", "Run rexctl logs edge to see why.")
		}
		return pass("Fluxer responds inside the edge container.")
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
			return pass("Fluxer responds on this machine.")
		}
		if strings.HasPrefix(u, "https://") {
			answered = true
			continue
		}
		if !answered && resp.StatusCode >= 300 && resp.StatusCode < 400 {
			return pass("Fluxer responds on this machine.")
		}
	}
	if answered {
		return fail("Fluxer's web server returns an error.", "Run rexctl logs api to see why.")
	}
	if !e.Native && answersInsideEdge(ctx, e) {
		return warn("Fluxer responds inside the edge container, but not on this machine's ports 80 and 443.", "That's fine if your own reverse proxy forwards to the edge container. Run rexctl proxy on to tell rexctl about it.")
	}
	if e.Native {
		return fail("Fluxer doesn't respond on this machine.", "Run rexctl logs to see why.")
	}
	return fail("Fluxer doesn't respond on this machine.", "Run rexctl logs edge to see why.")
}

func (e *Env) viaEdge(ctx context.Context, path string) (int, string, error) {
	client := e.localClient()
	err := errNoProject
	for _, u := range e.healthURLs() {
		req, rerr := http.NewRequestWithContext(ctx, http.MethodGet, strings.TrimSuffix(u, "/_health")+path, nil)
		if rerr != nil {
			err = rerr
			continue
		}
		req.Host = e.Domain
		resp, derr := client.Do(req)
		if derr != nil {
			err = derr
			continue
		}
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1<<20))
		resp.Body.Close()
		return resp.StatusCode, string(body), nil
	}
	return 0, "", err
}

func EdgeStarting(ctx context.Context, e *Env) bool {
	e.fillAddress()
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	code, body, err := e.viaEdge(ctx, "/_health")
	return err == nil && code == http.StatusServiceUnavailable && strings.HasPrefix(body, "Fluxer is starting")
}

func (e *Env) edgeUp(ctx context.Context) bool {
	if e.Native {
		return e.stack().UnitState(ctx) == "active"
	}
	states, err := e.serviceStates(ctx)
	return err == nil && slices.ContainsFunc(states, func(s ServiceState) bool { return s.Name == "edge" && s.Running() })
}

func answersInsideEdge(ctx context.Context, e *Env) bool {
	probe := "http://127.0.0.1:8088/_edge"
	if e.mode() == modeProxy {
		probe = "http://127.0.0.1:8080/_health"
	}
	_, err := e.exec(ctx, "edge", "", "wget", "-q", "-O", "/dev/null", probe)
	return err == nil
}
