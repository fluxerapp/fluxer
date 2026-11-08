// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func TestDecideBootService(t *testing.T) {
	never := func() (bool, error) {
		t.Fatal("asked without a terminal or with --yes")
		return false, nil
	}
	yes := func() (bool, error) { return true, nil }
	no := func() (bool, error) { return false, nil }
	cases := []struct {
		name              string
		want              checks.BootWant
		flag              string
		interactive, auto bool
		ask               func() (bool, error)
		on                bool
	}{
		{"docker fresh install asks and the answer is yes", checks.BootAsk, "", true, false, yes, true},
		{"docker fresh install declined", checks.BootAsk, "", true, false, no, false},
		{"docker without a terminal stays off", checks.BootAsk, "", false, false, never, false},
		{"docker with --yes stays off", checks.BootAsk, "", true, true, never, false},
		{"docker flag on", checks.BootAsk, instance.BootServiceOn, false, false, never, true},
		{"podman installs without the docker question", checks.BootOn, "", true, false, never, true},
		{"flag off wins over podman", checks.BootOn, instance.BootServiceOff, true, false, never, false},
		{"turned off stays off", checks.BootOff, "", true, false, never, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			got, err := decideBootService(tc.want, tc.flag, tc.interactive, tc.auto, tc.ask)
			if err != nil || got != tc.on {
				t.Fatalf("got %v, %v, want %v", got, err, tc.on)
			}
		})
	}
}

func TestHostStepsMode(t *testing.T) {
	cases := []struct {
		steps            int
		interactive, yes bool
		want             hostStepsAnswer
	}{
		{0, false, false, hostStepsRun},
		{2, true, true, hostStepsRun},
		{2, false, true, hostStepsRun},
		{2, true, false, hostStepsAsk},
		{2, false, false, hostStepsSkip},
	}
	for _, tc := range cases {
		if got := hostStepsMode(tc.steps, tc.interactive, tc.yes); got != tc.want {
			t.Errorf("hostStepsMode(%d, %v, %v) = %v, want %v", tc.steps, tc.interactive, tc.yes, got, tc.want)
		}
	}
}

func TestParseBootServiceFlag(t *testing.T) {
	for _, v := range []string{"", "on", "off"} {
		if _, err := parseBootServiceFlag(v); err != nil {
			t.Errorf("%q: %v", v, err)
		}
	}
	if _, err := parseBootServiceFlag("yes"); err == nil {
		t.Error("yes was accepted")
	}
}
