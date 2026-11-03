// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"errors"
	"os"
	"strings"
	"syscall"

	cerrdefs "github.com/containerd/errdefs"
	"github.com/moby/moby/api/types/jsonstream"
	"github.com/moby/moby/client"
)

type rule struct {
	class Class
	typed func(err error, op string) bool
	text  []string
}

var rules = []rule{
	{ClassDenied, func(err error, _ string) bool {
		return errors.Is(err, syscall.EACCES) || errors.Is(err, os.ErrPermission)
	}, []string{"permission denied while trying to connect", "access is denied"}},
	{ClassUnreachable, func(err error, _ string) bool {
		return client.IsErrConnectionFailed(err) || errors.Is(err, syscall.ECONNREFUSED) || errors.Is(err, syscall.ENOENT)
	}, []string{"cannot connect to the docker daemon", "is the docker daemon running"}},
	{ClassRateLimit, func(err error, _ string) bool {
		var js *jsonstream.Error
		return cerrdefs.IsResourceExhausted(err) || (errors.As(err, &js) && js.Code == 429)
	}, []string{"toomanyrequests", "429 too many requests", "too many requests", "pull rate limit"}},
	{ClassCredentials, nil, []string{"error getting credentials"}},
	{ClassAuth, func(err error, _ string) bool {
		return cerrdefs.IsUnauthorized(err) || cerrdefs.IsPermissionDenied(err)
	}, []string{"unauthorized", "authentication required", "denied:", "pull access denied"}},
	{ClassImageNotFound, func(err error, op string) bool {
		return op == "pull" && cerrdefs.IsNotFound(err)
	}, []string{"manifest unknown", "repository does not exist"}},
	{ClassDNS, nil, []string{"no such host", "server misbehaving", "temporary failure in name resolution"}},
	{ClassTLS, nil, []string{"x509:", "tls:", "server gave http response to https client"}},
	{ClassProxy, nil, []string{"proxyconnect"}},
	{ClassTimeout, func(err error, _ string) bool {
		return errors.Is(err, context.DeadlineExceeded)
	}, []string{"i/o timeout", "tls handshake timeout", "connection reset"}},
	{ClassDiskFull, func(err error, _ string) bool {
		return errors.Is(err, syscall.ENOSPC)
	}, []string{"no space left on device"}},
	{ClassPortInUse, nil, []string{"port is already allocated", "address already in use", "listen failed"}},
	{ClassAddressPools, nil, []string{"address pools have been fully subnetted"}},
	{ClassSubIDs, nil, []string{"insufficient uids or gids"}},
	{ClassDependencyFailed, nil, []string{"dependency failed to start", "timeout waiting for dependencies"}},
}

func ClassOf(err error) Class {
	if err == nil {
		return ClassOther
	}
	op := ""
	var e *Error
	if errors.As(err, &e) {
		if e.Class != ClassOther {
			return e.Class
		}
		op = e.Op
	}
	text := strings.ToLower(joinedText(err))
	for _, r := range rules {
		if r.typed != nil && r.typed(err, op) {
			if r.class == ClassUnreachable && strings.Contains(text, "permission denied") {
				return ClassDenied
			}
			return r.class
		}
		for _, t := range r.text {
			if !strings.Contains(text, t) {
				continue
			}
			if t == "listen failed" && !strings.Contains(text, "permission denied") {
				continue
			}
			return r.class
		}
	}
	return ClassOther
}

func joinedText(err error) string {
	var b strings.Builder
	var walk func(error)
	walk = func(err error) {
		if err == nil {
			return
		}
		b.WriteString(err.Error())
		b.WriteByte('\n')
		switch u := err.(type) {
		case interface{ Unwrap() []error }:
			for _, e := range u.Unwrap() {
				walk(e)
			}
		case interface{ Unwrap() error }:
			walk(u.Unwrap())
		}
	}
	walk(err)
	return b.String()
}
