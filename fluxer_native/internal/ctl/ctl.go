// SPDX-License-Identifier: AGPL-3.0-or-later

package ctl

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strings"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
	"github.com/fluxerapp/fluxer/fluxer_native/internal/supervise"
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

func Handler(sup *supervise.Supervisor, setJetStream func(on bool) error) http.Handler {
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
		if err := setJetStream(action == spec.ActionStart); err != nil {
			writeError(w, http.StatusInternalServerError, err)
			return
		}
		writeJSON(w, http.StatusOK, map[string]string{"status": "ok"})
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
	return fmt.Errorf("fluxerd answered %s: %s", resp.Status, strings.TrimSpace(string(data)))
}
