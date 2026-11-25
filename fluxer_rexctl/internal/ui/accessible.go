// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"fmt"
	"slices"
	"strconv"
	"strings"

	"golang.org/x/term"
)

func (u *UI) askAccessible(f Field) error {
	release := u.Hold()
	defer release()
	if d := f.description(); d != "" {
		u.emit(Inline(d) + "\n")
	}
	switch f.kind() {
	case KindConfirm:
		return u.accessibleConfirm(f)
	case KindSelect:
		return u.accessibleSelect(f)
	case KindMultiSelect:
		return u.accessibleMultiSelect(f)
	}
	return u.accessibleInput(f)
}

func (f Field) description() string {
	if f.DescriptionFunc != nil {
		return f.DescriptionFunc()
	}
	return f.Description
}

func (u *UI) accessibleConfirm(f Field) error {
	hint := "[y/N]"
	if *f.Bool {
		hint = "[Y/n]"
	}
	for {
		line, err := u.promptLine(Inline(f.Title) + " " + hint + " ")
		if err != nil {
			return err
		}
		switch strings.ToLower(line) {
		case "":
			return nil
		case "y", "yes":
			*f.Bool = true
			return nil
		case "n", "no":
			*f.Bool = false
			return nil
		}
		u.emit("Type y for yes or n for no.\n")
	}
}

func (u *UI) accessibleSelect(f Field) error {
	u.emit(Inline(f.Title) + "\n")
	def := 0
	for i, o := range f.Options {
		u.emit(fmt.Sprintf("%d. %s\n", i+1, Inline(optionLabel(o))))
		if o.Value == *f.Value {
			def = i + 1
		}
	}
	prompt := "Choose " + numberRange(1, len(f.Options)) + " "
	if def > 0 {
		prompt += fmt.Sprintf("[%d] ", def)
	}
	for {
		n, err := u.promptNumber(prompt, 1, len(f.Options), def)
		if err != nil {
			return err
		}
		v := f.Options[n-1].Value
		if f.Validate != nil {
			if err := f.Validate(v); err != nil {
				u.emit(Sentence(err.Error()) + "\n")
				continue
			}
		}
		*f.Value = v
		return nil
	}
}

func (u *UI) accessibleMultiSelect(f Field) error {
	u.emit(Inline(f.Title) + "\n")
	picked := slices.Clone(*f.Values)
	for {
		for i, o := range f.Options {
			mark := "[ ]"
			if slices.Contains(picked, o.Value) {
				mark = "[x]"
			}
			u.emit(fmt.Sprintf("%d. %s %s\n", i+1, mark, Inline(optionLabel(o))))
		}
		u.emit("0. Done\n")
		n, err := u.promptNumber("Choose a number to tick or untick, or 0 when done [0] ", 0, len(f.Options), 0)
		if err != nil {
			return err
		}
		if n == 0 {
			var out []string
			for _, o := range f.Options {
				if slices.Contains(picked, o.Value) {
					out = append(out, o.Value)
				}
			}
			*f.Values = out
			return nil
		}
		v := f.Options[n-1].Value
		if i := slices.Index(picked, v); i >= 0 {
			picked = slices.Delete(picked, i, i+1)
		} else {
			picked = append(picked, v)
		}
	}
}

func (u *UI) accessibleInput(f Field) error {
	secret := f.kind() == KindSecret || f.Secret
	prompt := Inline(f.Title) + " "
	if !secret && *f.Value != "" {
		prompt += "[" + Inline(*f.Value) + "] "
	}
	for {
		var line string
		var err error
		if secret {
			line, err = u.promptSecret(prompt, f)
		} else {
			line, err = u.promptLine(prompt)
		}
		if err != nil {
			return err
		}
		v := line
		if v == "" && !secret {
			v = *f.Value
		}
		if err := f.checkTyped(v); err != nil {
			u.emit(Sentence(err.Error()) + "\n")
			continue
		}
		*f.Value = v
		return nil
	}
}

func (f Field) checkTyped(v string) error {
	switch {
	case v == "" && f.Optional:
		return nil
	case v == "" && f.Default == "" && f.Validate != nil:
		if err := f.Validate(v); err != nil {
			return err
		}
		return nil
	case v == "" && f.Default == "":
		return promptError("Type an answer.")
	case v == "" || f.Validate == nil:
		return nil
	}
	return f.Validate(v)
}

func (u *UI) promptNumber(prompt string, low, high, def int) (int, error) {
	for {
		line, err := u.promptLine(prompt)
		if err != nil {
			return 0, err
		}
		if line == "" && def >= low {
			return def, nil
		}
		if n, err := strconv.Atoi(line); err == nil && n >= low && n <= high {
			return n, nil
		}
		u.emit("Type " + numberRange(low, high) + ".\n")
	}
}

func numberRange(low, high int) string {
	switch high - low {
	case 0:
		return strconv.Itoa(low)
	case 1:
		return fmt.Sprintf("%d or %d", low, high)
	}
	return fmt.Sprintf("a number from %d to %d", low, high)
}

func (u *UI) promptLine(prompt string) (string, error) {
	u.emit(prompt)
	line, err := u.readLine()
	if err != nil {
		return "", err
	}
	if isTerminal(u.opts.Stdin) {
		u.typedNewline()
	} else {
		u.emit(Inline(line) + "\n")
	}
	return line, nil
}

func (u *UI) promptSecret(prompt string, f Field) (string, error) {
	fd, ok := u.opts.Stdin.(interface{ Fd() uintptr })
	if !ok || !isTerminal(u.opts.Stdin) {
		return "", needsAnswer(f.Title, f.Flag)
	}
	u.emit(prompt)
	type result struct {
		b   []byte
		err error
	}
	done := make(chan result, 1)
	go func() {
		b, err := term.ReadPassword(int(fd.Fd()))
		done <- result{b, err}
	}()
	select {
	case r := <-done:
		u.emit("\n")
		if r.err != nil {
			return "", stoppedAtQuestion()
		}
		return string(r.b), nil
	case <-u.context().Done():
		u.emit("\n")
		return "", stoppedAtQuestion()
	}
}

func (u *UI) readLine() (string, error) {
	type result struct {
		line string
		err  error
	}
	done := make(chan result, 1)
	go func() {
		var b []byte
		one := make([]byte, 1)
		for {
			n, err := u.opts.Stdin.Read(one)
			if n == 1 {
				if one[0] == '\n' {
					done <- result{line: string(b)}
					return
				}
				b = append(b, one[0])
				continue
			}
			if err != nil {
				done <- result{err: err}
				return
			}
		}
	}()
	select {
	case r := <-done:
		if r.err != nil {
			u.emit("\n")
			return "", stoppedAtQuestion()
		}
		return strings.TrimSpace(r.line), nil
	case <-u.context().Done():
		u.emit("\n")
		return "", stoppedAtQuestion()
	}
}

func (u *UI) typedNewline() {
	u.mu.Lock()
	u.endsNL = true
	u.blank = false
	u.mu.Unlock()
}

func (u *UI) context() context.Context {
	if u.opts.Context == nil {
		return context.Background()
	}
	return u.opts.Context
}
