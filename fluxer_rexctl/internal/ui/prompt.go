// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"context"
	"errors"
	"fmt"
	"io"
	"strings"
	"sync/atomic"

	"charm.land/huh/v2"
	"github.com/spf13/cobra"
)

type Option struct {
	Label string
	Value string
}

type Field struct {
	Flag     string
	Title    string
	Value    *string
	Options  []Option
	Secret   bool
	Default  string
	Optional bool
	Validate func(string) error
}

var errNoValue = errors.New("prompt field has no value pointer")

type promptError string

func (e promptError) Error() string { return string(e) }

func (u *UI) Ask(cmd *cobra.Command, fields ...Field) error {
	for _, f := range fields {
		if f.Value == nil {
			return errNoValue
		}
		if f.Default == "" {
			f.Default = flagDefault(cmd, f.Flag)
		}
		if flagPassed(cmd, f.Flag) {
			if err := f.check(*f.Value); err != nil {
				return Usage(sentence(err.Error()))
			}
			continue
		}
		if !u.interactive {
			if *f.Value == "" {
				*f.Value = f.Default
			}
			if *f.Value == "" {
				if f.Optional {
					continue
				}
				return f.missing()
			}
			if err := f.check(*f.Value); err != nil {
				return Usage(sentence(err.Error()))
			}
			continue
		}
		if *f.Value == "" {
			*f.Value = f.Default
		}
		if err := u.runForm(u.fieldGroup(f)); err != nil {
			return err
		}
		if *f.Value == "" && !f.Optional {
			*f.Value = f.Default
		}
		u.echo(f.Title, f.answer(*f.Value))
	}
	return nil
}

func (f Field) answer(v string) string {
	if f.Secret {
		return strings.Repeat("•", min(len([]rune(v)), 12))
	}
	for _, o := range f.Options {
		if o.Value == v {
			return o.Label
		}
	}
	return v
}

func (u *UI) echo(question, answer string) {
	if u.accessible {
		return
	}
	th := u.themed()
	u.emit(th.Heading.Render(question) + "\n" + th.Accent.Render(">") + " " + answer + "\n\n")
}

func flagPassed(cmd *cobra.Command, name string) bool {
	if cmd == nil || name == "" {
		return false
	}
	fl := cmd.Flags().Lookup(name)
	return fl != nil && fl.Changed
}

func flagDefault(cmd *cobra.Command, name string) string {
	if cmd == nil || name == "" {
		return ""
	}
	fl := cmd.Flags().Lookup(name)
	if fl == nil {
		return ""
	}
	switch fl.DefValue {
	case "", "false", "[]":
		return ""
	}
	return fl.DefValue
}

func (f Field) check(v string) error {
	if len(f.Options) > 0 {
		ok := false
		for _, o := range f.Options {
			if o.Value == v {
				ok = true
				break
			}
		}
		if !ok {
			return fmt.Errorf("%s isn't one of %s", quoteValue(v), optionList(f.Options))
		}
	}
	if f.Validate != nil {
		return f.Validate(v)
	}
	return nil
}

func (f Field) missing() error {
	if f.Flag == "" {
		return Usage(fmt.Sprintf("%s needs an answer.", f.Title))
	}
	return MissingFlag(f.Flag)
}

func quoteValue(v string) string {
	if v == "" {
		return "An empty value"
	}
	return `"` + v + `"`
}

func optionList(opts []Option) string {
	s := ""
	for i, o := range opts {
		switch {
		case i == 0:
		case i == len(opts)-1:
			s += " or "
		default:
			s += ", "
		}
		s += o.Value
	}
	return s
}

func (u *UI) fieldGroup(f Field) *huh.Group {
	if len(f.Options) > 0 {
		opts := make([]huh.Option[string], len(f.Options))
		for i, o := range f.Options {
			opts[i] = huh.NewOption(o.Label, o.Value)
		}
		sel := huh.NewSelect[string]().Title(f.Title).Options(opts...).Value(f.Value)
		if f.Validate != nil {
			sel = sel.Validate(f.Validate)
		}
		return huh.NewGroup(sel)
	}
	in := huh.NewInput().Title(f.Title).Value(f.Value)
	if f.Secret {
		in = in.EchoMode(huh.EchoModePassword)
	}
	validate := f.Validate
	in = in.Validate(func(v string) error {
		if v == "" && f.Optional {
			return nil
		}
		if v == "" && f.Default == "" {
			return promptError("Type an answer.")
		}
		if v == "" {
			return nil
		}
		if validate != nil {
			return validate(v)
		}
		return nil
	})
	return huh.NewGroup(in)
}

type eofReader struct {
	r   io.Reader
	eof atomic.Bool
}

func (e *eofReader) Read(p []byte) (int, error) {
	n, err := e.r.Read(p)
	if err != nil {
		e.eof.Store(true)
	}
	return n, err
}

type fdEOFReader struct {
	*eofReader
	fd uintptr
}

func (f fdEOFReader) Fd() uintptr { return f.fd }

func (u *UI) runForm(groups ...*huh.Group) error {
	ctx := u.opts.Context
	if ctx == nil {
		ctx = context.Background()
	}
	u.themed()
	dark := u.dark
	in := &eofReader{r: u.opts.Stdin}
	input := u.opts.Stdin
	if u.accessible {
		input = in
		if f, ok := u.opts.Stdin.(interface{ Fd() uintptr }); ok {
			input = fdEOFReader{eofReader: in, fd: f.Fd()}
		}
	}
	form := huh.NewForm(groups...).
		WithTheme(huh.ThemeFunc(func(bool) *huh.Styles { return HuhTheme(dark) })).
		WithAccessible(u.accessible).
		WithShowHelp(false).
		WithInput(input).
		WithOutput(u.opts.Stderr)
	u.mu.Lock()
	if u.spin != nil {
		u.spin.clear()
	}
	u.mu.Unlock()
	var err error
	if u.accessible {
		done := make(chan error, 1)
		go func() { done <- form.RunWithContext(ctx) }()
		select {
		case err = <-done:
		case <-ctx.Done():
			return &ExitError{Code: ExitInterrupted}
		}
	} else {
		err = form.RunWithContext(ctx)
	}
	if errors.Is(err, huh.ErrUserAborted) || ctx.Err() != nil || (u.accessible && in.eof.Load()) {
		return &ExitError{Code: ExitInterrupted}
	}
	return err
}

func (u *UI) Confirm(question string, def bool) (bool, error) {
	if u.opts.Yes {
		return true, nil
	}
	if !u.interactive {
		return false, MissingFlag("yes")
	}
	v := def
	c := huh.NewConfirm().Title(question).Affirmative("Yes").Negative("No").Value(&v)
	if err := u.runForm(huh.NewGroup(c)); err != nil {
		return false, err
	}
	answer := "No"
	if v {
		answer = "Yes"
	}
	u.echo(question, answer)
	return v, nil
}

func (u *UI) ConfirmDestructive(what, domain, confirm string) error {
	if strings.TrimSpace(domain) == "" {
		return Refused("rexctl can't read your instance's domain to ask you to confirm.", "Run rexctl doctor to see what's wrong.")
	}
	if confirm != "" {
		if confirm == domain {
			return nil
		}
		return Refused(fmt.Sprintf("%s doesn't match %s.", confirm, domain), fmt.Sprintf("Pass --confirm %s.", domain))
	}
	if !u.interactive {
		return &ExitError{Code: ExitUsage, Msg: what, Fix: fmt.Sprintf("Pass --confirm %s to go ahead.", domain)}
	}
	if u.accessible {
		u.emit(what + "\n")
	}
	var typed string
	in := huh.NewInput().
		Title(fmt.Sprintf("Type %s to confirm.", domain)).
		Description(what).
		Value(&typed).
		Validate(func(v string) error {
			if v != domain {
				return promptError(fmt.Sprintf("Type %s exactly as shown.", domain))
			}
			return nil
		})
	if err := u.runForm(huh.NewGroup(in)); err != nil {
		return err
	}
	if strings.TrimSpace(typed) != domain {
		return Refused("Stopped without changing anything.", "")
	}
	u.echo(fmt.Sprintf("Type %s to confirm.", domain), typed)
	return nil
}
