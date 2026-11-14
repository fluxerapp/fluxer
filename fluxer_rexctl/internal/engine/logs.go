// SPDX-License-Identifier: AGPL-3.0-or-later

package engine

import (
	"context"
	"fmt"
	"io"
	"strconv"
	"sync"

	"github.com/docker/compose/v5/pkg/api"
)

type logWriter struct {
	mu sync.Mutex
	w  io.Writer
}

func (l *logWriter) write(container, msg string) {
	l.mu.Lock()
	defer l.mu.Unlock()
	fmt.Fprintf(l.w, "%s | %s\n", serviceOf("", container), msg)
}

func (l *logWriter) Log(container, msg string) { l.write(container, msg) }

func (l *logWriter) Err(container, msg string) { l.write(container, msg) }

func (l *logWriter) Status(string, string) {}

func (p *Project) Logs(ctx context.Context, o LogsOptions, services ...string) error {
	c, err := p.Engine.conn()
	if err != nil {
		return err
	}
	opts := api.LogOptions{Services: services, Follow: o.Follow, Since: o.Since}
	if o.Tail > 0 {
		opts.Tail = strconv.Itoa(o.Tail)
	}
	err = c.svc.Logs(ctx, p.Name, &logWriter{w: orDiscard(o.Stdout)}, opts)
	if err != nil && ctx.Err() != nil {
		return ctx.Err()
	}
	return p.wrap("logs", err)
}
