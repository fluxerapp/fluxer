// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"bufio"
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"sync"
)

var ErrNeedsRoot = errors.New("host: needs root or sudo")

var (
	holdMu   sync.Mutex
	holdTerm = func() func() { return func() {} }
)

func SetTerminalHold(hold func() func()) {
	holdMu.Lock()
	holdTerm = hold
	holdMu.Unlock()
}

func authenticate(ctx context.Context, sudo string) error {
	if exec.CommandContext(ctx, sudo, "-n", "true").Run() == nil {
		return nil
	}
	holdMu.Lock()
	hold := holdTerm
	holdMu.Unlock()
	release := hold()
	defer release()
	cmd := exec.CommandContext(ctx, sudo, "-v")
	cmd.Stdin, cmd.Stdout, cmd.Stderr = os.Stdin, os.Stderr, os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("sudo: %w", err)
	}
	return nil
}

func privileged(name string, args ...string) (string, []string, error) {
	if isRoot() {
		return name, args, nil
	}
	sudo, err := exec.LookPath("sudo")
	if err != nil {
		return "", nil, ErrNeedsRoot
	}
	return sudo, append([]string{name}, args...), nil
}

func output(ctx context.Context, quiet bool, name string, args ...string) ([]byte, error) {
	bin, argv, err := privileged(name, args...)
	if err != nil {
		return nil, err
	}
	if quiet && bin != name {
		argv = append([]string{"-n"}, argv...)
	}
	if !quiet && bin != name {
		if err := authenticate(ctx, bin); err != nil {
			return nil, err
		}
	}
	cmd := exec.CommandContext(ctx, bin, argv...)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if !quiet {
		cmd.Stdin = os.Stdin
	}
	out, err := cmd.Output()
	if err != nil {
		return out, fmt.Errorf("%s %s: %w: %s", name, strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return out, nil
}

func stream(ctx context.Context, log func(string), name string, args ...string) error {
	bin, argv, err := privileged(name, args...)
	if err != nil {
		return err
	}
	if bin != name {
		if err := authenticate(ctx, bin); err != nil {
			return err
		}
	}
	cmd := exec.CommandContext(ctx, bin, argv...)
	cmd.Stdin = os.Stdin
	pr, pw := io.Pipe()
	cmd.Stdout = pw
	cmd.Stderr = pw
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		s := bufio.NewScanner(pr)
		s.Buffer(make([]byte, 64*1024), 1024*1024)
		for s.Scan() {
			if log != nil {
				log(s.Text())
			}
		}
		_, _ = io.Copy(io.Discard, pr)
	}()
	err = cmd.Run()
	_ = pw.Close()
	wg.Wait()
	if err != nil {
		return fmt.Errorf("%s %s: %w", name, strings.Join(args, " "), err)
	}
	return nil
}

func has(name string) bool {
	_, err := exec.LookPath(name)
	return err == nil
}
