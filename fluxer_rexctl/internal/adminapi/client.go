// SPDX-License-Identifier: AGPL-3.0-or-later

package adminapi

import (
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"strings"
	"sync"
)

const SchemaMajor = 1

type Client struct {
	t        Transport
	Version  string
	OnSkew   func(apiVersion string)
	skewOnce sync.Once
}

func New(t Transport, version string) *Client { return &Client{t: t, Version: version} }

func (c *Client) Transport() Transport { return c.t }

type RawBody struct {
	ContentType string
	Data        []byte
}

func (c *Client) Call(ctx context.Context, method, path string, query map[string]string, in, out any) error {
	if !strings.HasPrefix(path, "/admin/") {
		return fmt.Errorf("adminapi: %s is not an admin path", path)
	}
	req := Request{Target: TargetOperator, Method: method, Path: path, Query: query}
	return c.do(ctx, req, in, out)
}

func (c *Client) CallWithHeaders(ctx context.Context, method, path string, query, headers map[string]string, in, out any) error {
	if !strings.HasPrefix(path, "/admin/") {
		return fmt.Errorf("adminapi: %s is not an admin path", path)
	}
	req := Request{Target: TargetOperator, Method: method, Path: path, Query: query, Headers: headers}
	return c.do(ctx, req, in, out)
}

func (c *Client) Loopback(ctx context.Context, method, path string, headers map[string]string, in, out any) error {
	req := Request{Target: TargetLoopback, Method: method, Path: path, Headers: headers}
	return c.do(ctx, req, in, out)
}

func (c *Client) do(ctx context.Context, req Request, in, out any) error {
	if in == nil && req.Method == http.MethodDelete && req.Target == TargetOperator {
		in = json.RawMessage("{}")
	}
	switch body := in.(type) {
	case nil:
	case RawBody:
		req.BodyBase64 = base64.StdEncoding.EncodeToString(body.Data)
		if req.Headers == nil {
			req.Headers = map[string]string{}
		}
		req.Headers["Content-Type"] = body.ContentType
	case json.RawMessage:
		req.Body = body
	default:
		data, err := json.Marshal(body)
		if err != nil {
			return err
		}
		req.Body = data
	}
	resp, err := c.t.Do(ctx, req)
	if err != nil {
		return err
	}
	if resp.RelayError != "" || resp.Status == 0 {
		return &RelayError{Detail: firstLine(resp.RelayError, "no response from the api")}
	}
	c.checkSkew(resp.APIVersion)
	if resp.SchemaMajor != 0 && resp.SchemaMajor != SchemaMajor {
		return ErrSchemaMismatch
	}
	if resp.Status >= 400 {
		return decodeAPIError(resp)
	}
	if out == nil || len(bytes.TrimSpace(resp.Body)) == 0 || string(bytes.TrimSpace(resp.Body)) == "null" {
		return nil
	}
	if err := json.Unmarshal(resp.Body, out); err != nil {
		return fmt.Errorf("adminapi: decode %s %s: %w", req.Method, req.Path, err)
	}
	return nil
}

func (c *Client) checkSkew(apiVersion string) {
	if apiVersion == "" || c.Version == "" || apiVersion == c.Version || c.Version == "dev" {
		return
	}
	c.skewOnce.Do(func() {
		if c.OnSkew != nil {
			c.OnSkew(apiVersion)
		}
	})
}

func decodeAPIError(resp *Response) error {
	e := &APIError{Status: resp.Status, Body: resp.Body}
	var body struct {
		Code    string       `json:"code"`
		Message string       `json:"message"`
		Errors  []FieldError `json:"errors"`
	}
	if err := json.Unmarshal(resp.Body, &body); err == nil {
		e.Code, e.Message, e.Fields = body.Code, body.Message, body.Errors
		return e
	}
	var text string
	if err := json.Unmarshal(resp.Body, &text); err == nil {
		e.Message = firstLine(text, "")
	}
	return e
}

func IsStatus(err error, status int) bool {
	var e *APIError
	return errors.As(err, &e) && e.Status == status
}
