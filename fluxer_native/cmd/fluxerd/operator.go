// SPDX-License-Identifier: AGPL-3.0-or-later

package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/user"
	"strings"
)

const (
	operatorClientHeader   = "X-Fluxer-Operator-Client"
	operatorHostUserHeader = "X-Fluxer-Operator-Host-User"
)

type operator struct {
	hc       *http.Client
	hostUser string
	socket   string
	place    runtimeLookup
	instead  string
}

func operatorClient(socket string) operator {
	return operator{
		socket: socket,
		hc: &http.Client{Transport: &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, "unix", socket)
		}}},
		hostUser: hostUser(),
	}
}

func hostUser() string {
	for _, key := range []string{"SUDO_USER", "USER"} {
		if v := strings.TrimSpace(os.Getenv(key)); v != "" {
			return v
		}
	}
	if u, err := user.Current(); err == nil {
		return u.Username
	}
	return ""
}

type fieldError struct {
	Path    string `json:"path"`
	Message string `json:"message"`
}

type operatorError struct {
	Method, Path, Status, Code, Message string
	StatusCode                          int
	Fields                              []fieldError
}

func (e *operatorError) Error() string {
	var fields []string
	for _, f := range e.Fields {
		if msg := strings.TrimSpace(f.Message); msg != "" && f.Path != "" {
			fields = append(fields, "Fluxer didn't accept the "+strings.ReplaceAll(f.Path, "_", " ")+". "+msg)
		}
	}
	if len(fields) > 0 {
		return strings.Join(fields, " ")
	}
	return strings.TrimSpace(fmt.Sprintf("%s %s failed with %s. %s", e.Method, e.Path, e.Status, e.Message))
}

func (o operator) call(ctx context.Context, method, path string, in, out any) error {
	var body io.Reader
	if in != nil {
		data, err := json.Marshal(in)
		if err != nil {
			return err
		}
		body = bytes.NewReader(data)
	}
	req, err := http.NewRequestWithContext(ctx, method, "http://operator"+path, body)
	if err != nil {
		return err
	}
	if in != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	req.Header.Set(operatorClientHeader, "fluxerd")
	if o.hostUser != "" {
		req.Header.Set(operatorHostUserHeader, o.hostUser)
	}
	resp, err := o.hc.Do(req)
	if err != nil {
		return o.place.unreachable(o.socket, err, o.instead)
	}
	defer resp.Body.Close()
	data, _ := io.ReadAll(io.LimitReader(resp.Body, 1<<20))
	if resp.StatusCode < 200 || resp.StatusCode > 299 {
		var apiErr struct {
			Code    string       `json:"code"`
			Message string       `json:"message"`
			Errors  []fieldError `json:"errors"`
		}
		_ = json.Unmarshal(data, &apiErr)
		return &operatorError{Method: method, Path: path, Status: resp.Status, StatusCode: resp.StatusCode, Code: apiErr.Code, Message: strings.TrimSpace(apiErr.Message), Fields: apiErr.Errors}
	}
	if out == nil {
		return nil
	}
	return json.Unmarshal(data, out)
}
