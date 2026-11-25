// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"errors"
	"strings"
)

type Step struct {
	Do      string                      `json:"do"`
	Command string                      `json:"command,omitempty"`
	Undo    string                      `json:"undo,omitempty"`
	Run     func(context.Context) error `json:"-"`
}

func RunSteps(ctx context.Context, steps []Step) error {
	for i, s := range steps {
		if s.Run == nil {
			continue
		}
		if err := s.Run(ctx); err != nil {
			if errors.Is(err, ErrNoSudo) {
				return &NoSudoError{Command: rootCommands(steps[i:])}
			}
			return err
		}
	}
	return nil
}

func rootCommands(steps []Step) string {
	if steps[0].Command == "" {
		return ""
	}
	cmds := []string{steps[0].Command}
	for _, s := range steps[1:] {
		switch {
		case s.Run == nil:
		case s.Command == "":
			return ""
		case strings.HasPrefix(s.Command, "sudo "):
			cmds = append(cmds, s.Command)
		}
	}
	return strings.Join(cmds, " && ")
}

func rootCmd(s string) string {
	if isRoot() {
		return s
	}
	return "sudo " + s
}

func joinCommands(steps []Step) string {
	var cmds []string
	for _, s := range steps {
		if s.Command != "" {
			cmds = append(cmds, s.Command)
		}
	}
	return strings.Join(cmds, " && ")
}
