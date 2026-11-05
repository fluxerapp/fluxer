// SPDX-License-Identifier: AGPL-3.0-or-later

package host

import (
	"context"
	"strings"
)

type Step struct {
	Do      string                      `json:"do"`
	Command string                      `json:"command,omitempty"`
	Undo    string                      `json:"undo,omitempty"`
	Run     func(context.Context) error `json:"-"`
}

func RunSteps(ctx context.Context, steps []Step) error {
	for _, s := range steps {
		if s.Run == nil {
			continue
		}
		if err := s.Run(ctx); err != nil {
			return err
		}
	}
	return nil
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
