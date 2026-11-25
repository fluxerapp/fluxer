// SPDX-License-Identifier: AGPL-3.0-or-later

package ctl

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"slices"
	"strings"
	"sync/atomic"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/procstat"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/supervise"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/updates"
)

func Listen(path string) (net.Listener, error) {
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		return nil, err
	}
	ln, err := net.Listen("unix", path)
	if err != nil {
		return nil, err
	}
	if err := os.Chmod(path, 0o600); err != nil {
		ln.Close()
		return nil, err
	}
	return ln, nil
}

type Swap struct {
	h atomic.Pointer[http.Handler]
}

func (s *Swap) Set(h http.Handler) { s.h.Store(&h) }

func (s *Swap) ServeHTTP(w http.ResponseWriter, r *http.Request) { (*s.h.Load()).ServeHTTP(w, r) }

func Waiting(status string) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("GET "+spec.PathHealth, func(w http.ResponseWriter, r *http.Request) {
		writeJSON(w, http.StatusServiceUnavailable, spec.Health{Status: status})
	})
	mux.HandleFunc("POST "+spec.PathReload, func(w http.ResponseWriter, r *http.Request) {
		writeError(w, http.StatusConflict, errors.New(status))
	})
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		writeError(w, http.StatusServiceUnavailable, errors.New(status))
	})
	return mux
}

type Hooks struct {
	JetStream   func(on bool) error
	Maintenance func(on bool) error
	Reload      func(ctx context.Context) (spec.Reloaded, error)
	Update      func(ctx context.Context, check bool) (updates.Report, error)
}

func Handler(sup *supervise.Supervisor, hooks Hooks) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("GET "+spec.PathPs, func(w http.ResponseWriter, r *http.Request) {
		writeJSON(w, http.StatusOK, sup.Ps())
	})
	mux.HandleFunc("GET "+spec.PathHealth, func(w http.ResponseWriter, r *http.Request) {
		h := sup.Health()
		status := http.StatusOK
		if !h.Ready {
			status = http.StatusServiceUnavailable
		}
		writeJSON(w, status, h)
	})
	mux.HandleFunc("POST "+spec.PathSvc+"{name}/{action}", func(w http.ResponseWriter, r *http.Request) {
		err := sup.Act(r.Context(), r.PathValue("name"), r.PathValue("action"))
		switch {
		case err == nil:
			writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
		case errors.Is(err, supervise.ErrUnknown):
			writeError(w, http.StatusNotFound, err)
		case errors.Is(err, supervise.ErrAction):
			writeError(w, http.StatusBadRequest, err)
		case errors.Is(err, supervise.ErrStopping):
			writeError(w, http.StatusConflict, err)
		default:
			writeError(w, http.StatusInternalServerError, err)
		}
	})
	mux.HandleFunc("POST "+spec.PathJetStream+"{action}", func(w http.ResponseWriter, r *http.Request) {
		action := r.PathValue("action")
		if action != spec.ActionStart && action != spec.ActionStop {
			writeError(w, http.StatusBadRequest, fmt.Errorf("unknown JetStream action %q", action))
			return
		}
		if err := hooks.JetStream(action == spec.ActionStart); err != nil {
			writeError(w, http.StatusInternalServerError, err)
			return
		}
		writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
	})
	mux.HandleFunc("POST "+spec.PathMaintenance+"{action}", func(w http.ResponseWriter, r *http.Request) {
		action := r.PathValue("action")
		if action != spec.MaintenanceOn && action != spec.MaintenanceOff {
			writeError(w, http.StatusBadRequest, fmt.Errorf("unknown maintenance action %q", action))
			return
		}
		if err := hooks.Maintenance(action == spec.MaintenanceOn); err != nil {
			writeError(w, http.StatusInternalServerError, err)
			return
		}
		writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
	})
	mux.HandleFunc("POST "+spec.PathReload, func(w http.ResponseWriter, r *http.Request) {
		res, err := hooks.Reload(r.Context())
		switch {
		case err == nil:
			writeJSON(w, http.StatusOK, res)
		case errors.Is(err, supervise.ErrRestart):
			writeError(w, http.StatusConflict, err)
		case errors.Is(err, supervise.ErrStopping):
			writeError(w, http.StatusServiceUnavailable, err)
		default:
			writeError(w, http.StatusUnprocessableEntity, err)
		}
	})
	update := func(check bool) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			if hooks.Update == nil {
				writeError(w, http.StatusNotFound, errors.New("voice-only mode doesn't check for new releases"))
				return
			}
			rep, err := hooks.Update(r.Context(), check)
			if err != nil && rep.Running == "" {
				writeError(w, http.StatusInternalServerError, err)
				return
			}
			writeJSON(w, http.StatusOK, rep)
		}
	}
	mux.HandleFunc("GET "+spec.PathUpdate, update(false))
	mux.HandleFunc("POST "+spec.PathUpdateCheck, update(true))
	mux.HandleFunc("GET "+spec.PathStats, func(w http.ResponseWriter, r *http.Request) {
		pids := map[string]int{}
		for _, e := range sup.Ps() {
			pids[e.Name] = e.PID
		}
		stats, err := procstat.Sample(r.Context(), pids, statsWindow)
		if err != nil {
			writeError(w, http.StatusInternalServerError, err)
			return
		}
		slices.SortFunc(stats, func(a, b procstat.Stat) int { return strings.Compare(a.Name, b.Name) })
		writeJSON(w, http.StatusOK, stats)
	})
	mux.HandleFunc("GET "+spec.PathEvents, func(w http.ResponseWriter, r *http.Request) {
		events(w, r, sup)
	})
	mux.HandleFunc("GET "+spec.PathLogs+"{name}", func(w http.ResponseWriter, r *http.Request) {
		ring, err := sup.Logs(r.PathValue("name"))
		if err != nil {
			writeError(w, http.StatusNotFound, err)
			return
		}
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		if r.URL.Query().Get("follow") != "1" {
			for _, line := range ring.Lines() {
				io.WriteString(w, line+"\n")
			}
			return
		}
		backlog, lines, cancel := ring.Follow()
		defer cancel()
		flusher, _ := w.(http.Flusher)
		for _, line := range backlog {
			io.WriteString(w, line+"\n")
		}
		for {
			if flusher != nil {
				flusher.Flush()
			}
			select {
			case <-r.Context().Done():
				return
			case line := <-lines:
				if _, err := io.WriteString(w, line+"\n"); err != nil {
					return
				}
			}
		}
	})
	return mux
}

const (
	statsWindow    = 250 * time.Millisecond
	eventsCoalesce = 250 * time.Millisecond
	eventsKeepOpen = 15 * time.Second
)

func events(w http.ResponseWriter, r *http.Request, sup *supervise.Supervisor) {
	flusher, ok := w.(http.Flusher)
	if !ok {
		writeError(w, http.StatusInternalServerError, errors.New("streaming is not supported"))
		return
	}
	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-store")
	w.WriteHeader(http.StatusOK)
	var lastPs, lastHealth []byte
	keep := time.NewTicker(eventsKeepOpen)
	defer keep.Stop()
	for {
		changed := sup.Changes()
		ps, _ := json.Marshal(sup.Ps())
		health, _ := json.Marshal(sup.Health())
		if !bytes.Equal(ps, lastPs) {
			fmt.Fprintf(w, "event: %s\ndata: %s\n\n", spec.EventPs, ps)
			lastPs = ps
		}
		if !bytes.Equal(health, lastHealth) {
			fmt.Fprintf(w, "event: %s\ndata: %s\n\n", spec.EventHealth, health)
			lastHealth = health
		}
		flusher.Flush()
		select {
		case <-r.Context().Done():
			return
		case <-keep.C:
			if _, err := io.WriteString(w, ": keep open\n\n"); err != nil {
				return
			}
			flusher.Flush()
			continue
		case <-changed:
		}
		timer := time.NewTimer(eventsCoalesce)
		select {
		case <-r.Context().Done():
			timer.Stop()
			return
		case <-timer.C:
		}
	}
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(v)
}

func writeError(w http.ResponseWriter, status int, err error) {
	writeJSON(w, status, map[string]string{"error": err.Error()})
}

type Client struct {
	http *http.Client
}

func Dial(socket string) *Client {
	return &Client{http: &http.Client{Transport: &http.Transport{
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, "unix", socket)
		},
	}}}
}

func (c *Client) do(ctx context.Context, method, path string) (*http.Response, error) {
	req, err := http.NewRequestWithContext(ctx, method, "http://fluxerd"+path, nil)
	if err != nil {
		return nil, err
	}
	return c.http.Do(req)
}

func (c *Client) PS(ctx context.Context) ([]spec.PsEntry, error) {
	var out []spec.PsEntry
	return out, c.getJSON(ctx, spec.PathPs, &out, http.StatusOK)
}

func (c *Client) Health(ctx context.Context) (spec.Health, error) {
	var h spec.Health
	return h, c.getJSON(ctx, spec.PathHealth, &h, http.StatusOK, http.StatusServiceUnavailable)
}

func (c *Client) Update(ctx context.Context) (updates.Report, error) {
	var r updates.Report
	return r, c.getJSON(ctx, spec.PathUpdate, &r, http.StatusOK)
}

func (c *Client) UpdateCheck(ctx context.Context) (updates.Report, error) {
	var r updates.Report
	resp, err := c.do(ctx, http.MethodPost, spec.PathUpdateCheck)
	if err != nil {
		return r, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return r, responseError(resp)
	}
	return r, json.NewDecoder(resp.Body).Decode(&r)
}

func (c *Client) getJSON(ctx context.Context, path string, v any, ok ...int) error {
	resp, err := c.do(ctx, http.MethodGet, path)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	for _, s := range ok {
		if resp.StatusCode == s {
			return json.NewDecoder(resp.Body).Decode(v)
		}
	}
	return responseError(resp)
}

func (c *Client) Service(ctx context.Context, name, action string) error {
	return c.post(ctx, spec.PathSvc+name+"/"+action)
}

func (c *Client) JetStream(ctx context.Context, action string) error {
	return c.post(ctx, spec.PathJetStream+action)
}

func (c *Client) Maintenance(ctx context.Context, on bool) error {
	if on {
		return c.post(ctx, spec.PathMaintenance+spec.MaintenanceOn)
	}
	return c.post(ctx, spec.PathMaintenance+spec.MaintenanceOff)
}

func (c *Client) post(ctx context.Context, path string) error {
	resp, err := c.do(ctx, http.MethodPost, path)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return responseError(resp)
	}
	return nil
}

func (c *Client) Logs(ctx context.Context, name string, follow bool, w io.Writer) error {
	path := spec.PathLogs + name
	if follow {
		path += "?follow=1"
	}
	resp, err := c.do(ctx, http.MethodGet, path)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return responseError(resp)
	}
	_, err = io.Copy(w, resp.Body)
	if ctx.Err() != nil {
		return nil
	}
	return err
}

func responseError(resp *http.Response) error {
	var body struct {
		Error string `json:"error"`
	}
	data, _ := io.ReadAll(io.LimitReader(resp.Body, 4096))
	if json.Unmarshal(data, &body) == nil && body.Error != "" {
		return errors.New(body.Error)
	}
	return fmt.Errorf("fluxerd returned %s: %s", resp.Status, strings.TrimSpace(string(data)))
}
