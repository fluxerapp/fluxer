// SPDX-License-Identifier: AGPL-3.0-or-later

package native

import (
	"bufio"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

const (
	aptLogName = "apt.log"
	aptTail    = 15
)

type AptReporter interface {
	Title(title string)
	Percent(pct float64)
	LogPath(path string)
}

type AptError struct {
	Command string
	Log     string
	Tail    []string
	Err     error
}

func (e *AptError) Error() string {
	msg := fmt.Sprintf("native: %s: %v", e.Command, e.Err)
	if e.Log != "" {
		msg += " (full log in " + e.Log + ")"
	}
	if len(e.Tail) > 0 {
		msg += "\n" + strings.Join(e.Tail, "\n")
	}
	return msg
}

func (e *AptError) Unwrap() error { return e.Err }

type apt struct {
	r    AptReporter
	log  io.Writer
	path string
}

func (s *Stack) newApt(r AptReporter) (*apt, func()) {
	a := &apt{r: r, log: io.Discard}
	if err := os.MkdirAll(s.Inst.Path(instance.StateDirName), 0o700); err != nil {
		return a, func() {}
	}
	path := s.Inst.Path(instance.StateDirName, aptLogName)
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600)
	if err != nil {
		return a, func() {}
	}
	a.log, a.path = f, path
	if r != nil {
		r.LogPath(path)
	}
	return a, func() { f.Close() }
}

type tailWriter struct {
	mu    sync.Mutex
	w     io.Writer
	part  string
	lines []string
}

func (t *tailWriter) Write(p []byte) (int, error) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.w.Write(p)
	text := t.part + string(p)
	parts := strings.Split(text, "\n")
	t.part = parts[len(parts)-1]
	for _, line := range parts[:len(parts)-1] {
		if line = strings.TrimRight(line, " \r"); line != "" {
			t.lines = append(t.lines, line)
		}
	}
	if over := len(t.lines) - aptTail; over > 0 {
		t.lines = t.lines[over:]
	}
	return len(p), nil
}

func (t *tailWriter) tail() []string {
	t.mu.Lock()
	defer t.mu.Unlock()
	out := t.lines
	if p := strings.TrimSpace(t.part); p != "" {
		out = append(out, p)
	}
	if over := len(out) - aptTail; over > 0 {
		out = out[over:]
	}
	return out
}

type aptStage struct {
	title, downloading, installing string
}

const aptWaitDelay = 10 * time.Second

func (a *apt) run(ctx context.Context, st aptStage, args ...string) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	if a.r != nil {
		a.r.Title(st.title)
		a.r.Percent(-1)
	}
	argv := append([]string{"-o", "APT::Status-Fd=3"}, args...)
	cmd := exec.Command("apt-get", argv...)
	ownGroup(cmd)
	cmd.WaitDelay = aptWaitDelay
	cmd.Env = append(os.Environ(), "DEBIAN_FRONTEND=noninteractive")
	fmt.Fprintf(a.log, "$ apt-get %s\n", strings.Join(args, " "))
	out := &tailWriter{w: a.log}
	cmd.Stdout, cmd.Stderr = out, out
	statusR, statusW, err := os.Pipe()
	if err != nil {
		return err
	}
	cmd.ExtraFiles = []*os.File{statusW}
	err = cmd.Start()
	statusW.Close()
	if err != nil {
		statusR.Close()
		return err
	}
	done := make(chan struct{})
	go func() {
		defer close(done)
		a.status(statusR, st)
	}()
	exited := make(chan struct{})
	go func() {
		select {
		case <-ctx.Done():
			if a.r != nil {
				a.r.Title("Letting apt finish so the package database stays whole")
			}
		case <-exited:
		}
	}()
	err = cmd.Wait()
	close(exited)
	select {
	case <-done:
	case <-time.After(aptWaitDelay):
	}
	statusR.Close()
	<-done
	if err != nil {
		err = &AptError{Command: "apt-get " + strings.Join(args, " "), Log: a.path, Tail: out.tail(), Err: err}
	}
	if ctx.Err() != nil {
		return errors.Join(ctx.Err(), err)
	}
	return err
}

func (a *apt) status(r io.Reader, st aptStage) {
	sc := bufio.NewScanner(r)
	stage := ""
	for sc.Scan() {
		kind, pct, ok := parseAptStatus(sc.Text())
		if !ok || a.r == nil {
			continue
		}
		if kind != stage {
			stage = kind
			switch {
			case kind == "dlstatus" && st.downloading != "":
				a.r.Title(st.downloading)
			case kind == "pmstatus" && st.installing != "":
				a.r.Title(st.installing)
			}
		}
		a.r.Percent(pct)
	}
}

func parseAptStatus(line string) (string, float64, bool) {
	f := strings.SplitN(line, ":", 4)
	if len(f) < 3 || (f[0] != "dlstatus" && f[0] != "pmstatus") {
		return "", 0, false
	}
	pct, err := strconv.ParseFloat(f[2], 64)
	if err != nil {
		return "", 0, false
	}
	return f[0], min(max(pct, 0), 100), true
}
