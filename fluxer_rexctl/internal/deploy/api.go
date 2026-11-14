// SPDX-License-Identifier: AGPL-3.0-or-later

package deploy

import "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"

type ApplyOptions struct {
	Pull    bool
	Wait    bool
	Confirm func(Preview) bool
	OnEvent func(engine.Event)
	Failed  func(*engine.Project, error)
}

type Preview struct {
	Keys      []string
	Promised  []string
	Restarts  []string
	Removes   []string
	Networks  []string
	Unchanged bool
}

type Change struct {
	Keys      []string
	Restarted []string
	Removed   []string
	Applied   bool
	Reverted  bool
	Unchanged bool
	VoiceDown []string
}
