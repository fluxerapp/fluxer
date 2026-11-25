// SPDX-License-Identifier: AGPL-3.0-or-later

package ui

import (
	"runtime"
	"strings"
)

type Tier int

const (
	TierLive Tier = iota
	TierStill
	TierLines
	TierJSON
)

type GlyphSet int

const (
	GlyphsAuto GlyphSet = iota
	GlyphsUnicode
	GlyphsASCII
)

type ProgressFormat string

const (
	ProgressAuto  ProgressFormat = "auto"
	ProgressPlain ProgressFormat = "plain"
	ProgressJSON  ProgressFormat = "json"
)

var ciVars = []string{"CI", "GITHUB_ACTIONS", "GITLAB_CI", "BUILDKITE", "JENKINS_URL"}

func detectTier(opts Options, stderrTTY, accessible bool) Tier {
	getenv := opts.Getenv
	if opts.Progress == ProgressJSON {
		return TierJSON
	}
	lines := !stderrTTY || opts.Plain || accessible || opts.Progress == ProgressPlain || getenv("TERM") == "dumb"
	for _, k := range ciVars {
		lines = lines || getenv(k) != ""
	}
	switch {
	case lines:
		return TierLines
	case opts.NoAnimation || getenv("REXCTL_REDUCED_MOTION") != "":
		return TierStill
	}
	return TierLive
}

func forced(v string) bool { return v != "" && v != "0" }

func colourOn(opts Options, tier Tier, stderrTTY, accessible bool) bool {
	getenv := opts.Getenv
	switch {
	case tier == TierJSON || accessible || opts.NoColor:
		return false
	case forced(getenv("FORCE_COLOR")) || forced(getenv("CLICOLOR_FORCE")):
		return true
	case tier == TierLines:
		return false
	}
	return stderrTTY && !opts.NoColor && colourAllowed(getenv)
}

func detectGlyphs(opts Options, accessible bool) GlyphSet {
	if opts.Glyphs != GlyphsAuto {
		return opts.Glyphs
	}
	getenv := opts.Getenv
	switch strings.ToLower(getenv("REXCTL_GLYPHS")) {
	case "ascii":
		return GlyphsASCII
	case "unicode":
		return GlyphsUnicode
	}
	if accessible || getenv("TERM") == "linux" || getenv("TERM") == "dumb" {
		return GlyphsASCII
	}
	if runtime.GOOS == "windows" {
		if getenv("WT_SESSION") == "" && getenv("TERM_PROGRAM") == "" {
			return GlyphsASCII
		}
		return GlyphsUnicode
	}
	for _, k := range []string{"LC_ALL", "LC_CTYPE", "LANG"} {
		v := strings.ToLower(getenv(k))
		if strings.Contains(v, "utf-8") || strings.Contains(v, "utf8") {
			return GlyphsUnicode
		}
	}
	return GlyphsASCII
}
