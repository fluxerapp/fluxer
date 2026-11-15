// SPDX-License-Identifier: AGPL-3.0-or-later

package probe

import (
	"bufio"
	"bytes"
	"context"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const defaultTimeout = 5 * time.Second

var client = &http.Client{Transport: &http.Transport{Proxy: nil, DisableKeepAlives: true}}

func All(ctx context.Context, probes []spec.Probe) error {
	for _, p := range probes {
		if err := Check(ctx, p); err != nil {
			return fmt.Errorf("%s: %w", Describe(p), err)
		}
	}
	return nil
}

func Describe(p spec.Probe) string {
	var what string
	switch p.Kind {
	case spec.ProbeHTTP:
		what = "GET " + p.Addr + p.Path
	case spec.ProbeExec:
		what = strings.Join(p.Argv, " ")
	case spec.ProbeFileFresh:
		what = p.File
	default:
		what = string(p.Kind) + " probe"
	}
	if p.Name != "" {
		return p.Name + " (" + what + ")"
	}
	return what
}

func Check(ctx context.Context, p spec.Probe) error {
	if _, ok := ctx.Deadline(); !ok {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, defaultTimeout)
		defer cancel()
	}
	switch p.Kind {
	case spec.ProbeHTTP:
		return checkHTTP(ctx, p)
	case spec.ProbeExec:
		if len(p.Argv) == 0 {
			return fmt.Errorf("exec probe has no command")
		}
		out, err := exec.CommandContext(ctx, p.Argv[0], p.Argv[1:]...).CombinedOutput()
		if err != nil {
			return fmt.Errorf("%s: %w: %s", p.Argv[0], err, bytes.TrimSpace(out))
		}
		return nil
	case spec.ProbeFileFresh:
		st, err := os.Stat(p.File)
		if err != nil {
			return err
		}
		if age := time.Since(st.ModTime()); age > p.MaxAge {
			return fmt.Errorf("%s was last updated %s ago", p.File, age.Round(time.Second))
		}
		return nil
	case spec.ProbeFunc:
		return p.Func(ctx)
	}
	return fmt.Errorf("unknown probe kind %q", p.Kind)
}

func checkHTTP(ctx context.Context, p spec.Probe) error {
	want := p.Status
	if want == 0 {
		want = http.StatusOK
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, "http://"+p.Addr+p.Path, nil)
	if err != nil {
		return err
	}
	resp, err := client.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	_, _ = io.Copy(io.Discard, io.LimitReader(resp.Body, 64<<10))
	if resp.StatusCode != want {
		return fmt.Errorf("GET %s returned %d, want %d", p.Path, resp.StatusCode, want)
	}
	return nil
}

func Valkey(addr, password string) func(context.Context) error {
	return func(ctx context.Context) error {
		var d net.Dialer
		conn, err := d.DialContext(ctx, "tcp", addr)
		if err != nil {
			return err
		}
		defer conn.Close()
		if deadline, ok := ctx.Deadline(); ok {
			_ = conn.SetDeadline(deadline)
		}
		r := bufio.NewReader(conn)
		if password != "" {
			if err := command(conn, r, "+OK", "AUTH", password); err != nil {
				return err
			}
		}
		return command(conn, r, "+PONG", "PING")
	}
}

func command(w io.Writer, r *bufio.Reader, want string, args ...string) error {
	var b strings.Builder
	fmt.Fprintf(&b, "*%d\r\n", len(args))
	for _, a := range args {
		fmt.Fprintf(&b, "$%d\r\n%s\r\n", len(a), a)
	}
	if _, err := io.WriteString(w, b.String()); err != nil {
		return err
	}
	line, err := r.ReadString('\n')
	if err != nil {
		return err
	}
	if got := strings.TrimRight(line, "\r\n"); got != want {
		return fmt.Errorf("valkey %s replied %q", args[0], got)
	}
	return nil
}
