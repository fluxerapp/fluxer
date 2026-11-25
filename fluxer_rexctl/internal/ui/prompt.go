// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"errors"
	"fmt"
	"slices"
	"strconv"
	"strings"

	tea "charm.land/bubbletea/v2"
	"charm.land/huh/v2"
	"charm.land/lipgloss/v2"
	"github.com/charmbracelet/colorprofile"
	"github.com/charmbracelet/x/ansi"
	"github.com/spf13/cobra"
)

type FieldKind int

const (
	KindAuto FieldKind = iota
	KindInput
	KindSelect
	KindConfirm
	KindSecret
	KindMultiSelect
)

type Option struct {
	Label string
	Value string
	Hint  string
}

type Field struct {
	Flag            string
	Title           string
	Value           *string
	Options         []Option
	Secret          bool
	Default         string
	Optional        bool
	Validate        func(string) error
	Kind            FieldKind
	Bool            *bool
	Values          *[]string
	Description     string
	DescriptionFunc func() string
	Placeholder     string
	Hide            func() bool
	FlagLabel       string
}

var errNoValue = errors.New("prompt field has no value pointer")

type promptError string

func (e promptError) Error() string { return string(e) }

func (f Field) kind() FieldKind {
	switch {
	case f.Kind != KindAuto:
		return f.Kind
	case len(f.Options) > 0:
		return KindSelect
	case f.Secret:
		return KindSecret
	}
	return KindInput
}

func (f Field) hasValue() bool {
	switch f.kind() {
	case KindConfirm:
		return f.Bool != nil
	case KindMultiSelect:
		return f.Values != nil
	}
	return f.Value != nil
}

func (u *UI) Ask(cmd *cobra.Command, fields ...Field) error {
	for _, f := range fields {
		if !f.hasValue() {
			return errNoValue
		}
		if f.Hide != nil && f.Hide() {
			continue
		}
		ask, err := u.resolve(cmd, &f)
		if err != nil {
			return err
		}
		if !ask {
			continue
		}
		if err := u.askField(f); err != nil {
			return err
		}
		f.settle()
		u.echo(f.Title, f.answerText(u.glyphs.Bullet))
	}
	return nil
}

func (u *UI) askField(f Field) error {
	if u.accessible {
		return u.askAccessible(f)
	}
	return u.runForm(huh.NewGroup(u.fieldHuh(f, nil)))
}

func (u *UI) resolve(cmd *cobra.Command, f *Field) (bool, error) {
	switch f.kind() {
	case KindConfirm:
		if flagPassed(cmd, f.Flag) {
			return false, nil
		}
		if f.Default != "" {
			if v, err := strconv.ParseBool(f.Default); err == nil {
				*f.Bool = v
			}
		}
		return u.interactive, nil
	case KindMultiSelect:
		if flagPassed(cmd, f.Flag) {
			for _, v := range *f.Values {
				if err := f.check(v); err != nil {
					return false, f.flagError(v, err)
				}
			}
			return false, nil
		}
		return u.interactive, nil
	}
	if f.Default == "" {
		f.Default = flagDefault(cmd, f.Flag)
	}
	if flagPassed(cmd, f.Flag) {
		if err := f.check(*f.Value); err != nil {
			return false, f.flagError(*f.Value, err)
		}
		return false, nil
	}
	if *f.Value == "" {
		*f.Value = f.Default
	}
	if !u.interactive {
		if *f.Value == "" {
			if f.Optional {
				return false, nil
			}
			return false, f.missing()
		}
		if err := f.check(*f.Value); err != nil {
			return false, Usage(Sentence(err.Error()))
		}
		return false, nil
	}
	return true, nil
}

func (f Field) keepWhileHidden() func() {
	switch f.kind() {
	case KindConfirm:
		was := *f.Bool
		return func() {
			if f.Hide() {
				*f.Bool = was
			}
		}
	case KindMultiSelect:
		was := slices.Clone(*f.Values)
		return func() {
			if f.Hide() {
				*f.Values = was
			}
		}
	}
	was := *f.Value
	return func() {
		if f.Hide() {
			*f.Value = was
		}
	}
}

func (f Field) settle() {
	switch f.kind() {
	case KindConfirm, KindMultiSelect:
		return
	}
	if *f.Value == "" && !f.Optional {
		*f.Value = f.Default
	}
}

func (f Field) answerText(mask string) string {
	switch f.kind() {
	case KindConfirm:
		return yesNo(*f.Bool)
	case KindMultiSelect:
		var labels []string
		for _, v := range *f.Values {
			labels = append(labels, f.answer(v, mask))
		}
		if len(labels) == 0 {
			return "none"
		}
		return JoinAnd(labels)
	}
	return f.answer(*f.Value, mask)
}

func (f Field) flagError(v string, err error) error {
	if f.FlagLabel == "" {
		return Usage(Sentence(err.Error()))
	}
	return Usage(fmt.Sprintf("%s can't be %s. %s", f.FlagLabel, quoteValue(v), Sentence(err.Error())))
}

func (f Field) binding() any {
	switch f.kind() {
	case KindConfirm:
		return f.Bool
	case KindMultiSelect:
		return f.Values
	}
	return f.Value
}

type describer[T any] interface {
	Description(string) T
	DescriptionFunc(func() string, any) T
}

func describe[T any](d describer[T], f Field, bind any, self T, width int) T {
	if bind == nil {
		bind = f.binding()
	}
	switch {
	case f.DescriptionFunc != nil:
		return d.DescriptionFunc(func() string { return wordwrap(f.DescriptionFunc(), width) }, bind)
	case f.Description != "":
		return d.Description(wordwrap(f.Description, width))
	}
	return self
}

func (u *UI) describeWidth() int { return max(min(u.Width(), maxContentCol)-6, 20) }

func (u *UI) fieldHuh(f Field, bind any) huh.Field {
	switch f.kind() {
	case KindConfirm:
		c := huh.NewConfirm().Title(holdUnits(f.Title)).Affirmative("Yes").Negative("No").Value(f.Bool).WithButtonAlignment(lipgloss.Left)
		return describe[*huh.Confirm](c, f, bind, c, u.describeWidth())
	case KindMultiSelect:
		labels := u.optionLabels(f.Options, 6)
		opts := make([]huh.Option[string], len(f.Options))
		for i, o := range f.Options {
			opts[i] = huh.NewOption(labels[i], o.Value).Selected(slices.Contains(*f.Values, o.Value))
		}
		m := huh.NewMultiSelect[string]().Title(holdUnits(f.Title)).Options(opts...).Value(f.Values)
		return describe[*huh.MultiSelect[string]](m, f, bind, m, u.describeWidth())
	case KindSelect:
		labels := u.optionLabels(f.Options, 2)
		opts := make([]huh.Option[string], len(f.Options))
		for i, o := range f.Options {
			opts[i] = huh.NewOption(labels[i], o.Value)
		}
		sel := huh.NewSelect[string]().Title(holdUnits(f.Title)).Options(opts...).Value(f.Value)
		if f.Validate != nil {
			sel = sel.Validate(f.Validate)
		}
		return describe[*huh.Select[string]](sel, f, bind, sel, u.describeWidth())
	}
	in := huh.NewInput().Title(holdUnits(f.Title)).Value(f.Value)
	in = describe[*huh.Input](in, f, bind, in, u.describeWidth())
	if f.Placeholder != "" {
		in = in.Placeholder(f.Placeholder)
	}
	if f.kind() == KindSecret || f.Secret {
		in = in.EchoMode(huh.EchoModePassword)
	}
	return in.Validate(f.checkTyped)
}

func (u *UI) optionLabels(options []Option, mark int) []string {
	labelW := 0
	for _, o := range options {
		labelW = max(labelW, ansi.StringWidth(o.Label))
	}
	room := min(u.Width(), maxContentCol) - 10 - mark
	muted := u.themed().Muted
	out := make([]string, len(options))
	for i, o := range options {
		switch {
		case o.Hint == "":
			out[i] = o.Label
		case labelW+3+ansi.StringWidth(o.Hint) <= room:
			out[i] = padRight(o.Label, labelW) + "   " + muted.Render(o.Hint)
		default:
			out[i] = o.Label
		}
	}
	return out
}

func optionLabel(o Option) string {
	if o.Hint == "" {
		return o.Label
	}
	return o.Label + "  " + o.Hint
}

func yesNo(v bool) string {
	if v {
		return "Yes"
	}
	return "No"
}

func (f Field) answer(v, mask string) string {
	if f.Secret {
		return strings.Repeat(mask, min(len([]rune(v)), 12))
	}
	for _, o := range f.Options {
		if o.Value == v {
			return o.Label
		}
	}
	return v
}

func (u *UI) echo(question, answer string) {
	if u.accessible || u.silent() {
		return
	}
	if !u.rich() {
		u.emit(Inline(question) + " " + Inline(answer) + "\n")
		return
	}
	lk := u.look()
	line := lk.th.OK.Render(lk.g.cell(lk.g.OK)) + " " + Inline(question) + " " + lk.th.Accent.Render(Inline(answer))
	u.emit(strings.ReplaceAll(wordwrapKeepHyphens(line, min(u.Width(), maxContentCol)-2), "\n", "\n  ") + "\n")
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
	if f.Title == "" {
		return MissingFlag(f.Flag)
	}
	return needsAnswer(f.Title, f.Flag)
}

func needsAnswer(question, flag string) *ExitError {
	e := &ExitError{Code: ExitUsage, Msg: question + " rexctl can't ask that here."}
	switch flag {
	case "":
	case "yes":
		e.Fix = "Pass --yes to answer yes."
	default:
		e.Fix = "Pass --" + flag + " with the answer."
	}
	return e
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

func (u *UI) form(groups ...*huh.Group) *huh.Form {
	u.themed()
	dark := u.dark
	theme := huh.ThemeFunc(func(bool) *huh.Styles { return huhTheme(dark) })
	if !u.colour {
		theme = huh.ThemeFunc(huhPlainTheme)
	}
	return huh.NewForm(groups...).
		WithTheme(theme).
		WithWidth(min(u.Width(), maxContentCol)).
		WithViewHook(func(v tea.View) tea.View {
			v.Content = releaseUnits(v.Content)
			v.ProgramStatus = &tea.ProgramStatus{State: tea.ProgramStateBlocked, App: "rexctl", Kind: tea.ProgramStatusKindQuestion}
			return v
		})
}

func (u *UI) runForm(groups ...*huh.Group) error {
	form := u.form(groups...)
	ctx := u.context()
	out := u.opts.Stderr
	form = form.WithInput(u.opts.Stdin).WithOutput(out)
	if !u.colour {
		form = form.WithProgramOptions(tea.WithColorProfile(colorprofile.Ascii))
	}
	release := u.Hold()
	defer release()
	err := form.RunWithContext(ctx)
	if errors.Is(err, huh.ErrUserAborted) || ctx.Err() != nil {
		fmt.Fprintln(out)
		return stoppedAtQuestion()
	}
	return err
}

type Question struct {
	Text        string
	Detail      string
	Default     bool
	Destructive bool
	Domain      string
	Confirm     string
	Flag        string
}

func (u *UI) Confirm(question string, def bool) (bool, error) {
	return u.ConfirmWith(Question{Text: question, Default: def})
}

func (u *UI) ConfirmWith(q Question) (bool, error) {
	if q.Destructive {
		if err := u.ConfirmDestructive(q.Text, q.Domain, q.Confirm); err != nil {
			return false, err
		}
		return true, nil
	}
	flag := q.Flag
	if flag == "" {
		flag = "yes"
	}
	if u.opts.Yes && flag == "yes" {
		return true, nil
	}
	if !u.interactive {
		return false, needsAnswer(q.Text, flag)
	}
	v := q.Default
	if err := u.askField(Field{Title: q.Text, Description: q.Detail, Bool: &v, Kind: KindConfirm}); err != nil {
		return false, err
	}
	u.echo(q.Text, yesNo(v))
	return v, nil
}

func (u *UI) ConfirmDestructive(what, domain, confirm string) error {
	return u.confirmTyped(what, domain, confirm, what)
}

func (u *UI) confirmTyped(what, domain, confirm, detail string) error {
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
		fix := fmt.Sprintf("Pass --confirm %s to go ahead.", domain)
		if u.opts.Yes {
			fix += " --yes doesn't cover this question."
		}
		return &ExitError{Code: ExitUsage, Msg: what, Fix: fix}
	}
	var typed string
	title := fmt.Sprintf("Type %s to confirm.", domain)
	err := u.askField(Field{Title: title, Description: detail, Value: &typed, Kind: KindInput, Validate: func(v string) error {
		if v != domain {
			return promptError(fmt.Sprintf("Type %s exactly as shown.", domain))
		}
		return nil
	}})
	if err != nil {
		return err
	}
	u.echo(title, typed)
	return nil
}

type DestructiveCard struct {
	Title string
	Body  []string
	Goes  []KV
	Stays []KV
}

func (u *UI) Destructive(c DestructiveCard, domain, confirm string) error {
	if confirm == "" && !u.silent() {
		u.Card(destructiveCard(c))
	}
	what := c.Title
	if len(c.Body) > 0 {
		what = c.Body[0]
	}
	if u.silent() {
		return u.ConfirmDestructive(what, domain, confirm)
	}
	return u.confirmTyped(what, domain, confirm, "")
}

func destructiveCard(c DestructiveCard) Card {
	card := Card{Title: c.Title, Tone: ToneFail, Body: c.Body}
	w := 0
	for _, it := range append(slices.Clone(c.Goes), c.Stays...) {
		w = max(w, ansi.StringWidth(Inline(it.Key)))
	}
	list := func(label string, items []KV) {
		for i, it := range items {
			row := KV{Value: padRight(Inline(it.Key), w), Hint: it.Value}
			if i == 0 {
				row.Key = label
			}
			card.Rows = append(card.Rows, row)
		}
	}
	list("Goes", c.Goes)
	list("Stays", c.Stays)
	return card
}
