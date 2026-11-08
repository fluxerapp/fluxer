// SPDX-License-Identifier: AGPL-3.0-or-later

package checks

import (
	"testing"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
)

func TestBootServiceWant(t *testing.T) {
	cases := []struct {
		name              string
		pref              string
		podman, installed bool
		want              BootWant
	}{
		{"docker fresh install asks", "", false, false, BootAsk},
		{"docker keeps an existing service", "", false, true, BootOn},
		{"docker turned on", instance.BootServiceOn, false, false, BootOn},
		{"docker turned off", instance.BootServiceOff, false, true, BootOff},
		{"podman needs it", "", true, false, BootOn},
		{"podman turned off", instance.BootServiceOff, true, false, BootOff},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := BootServiceWant(tc.pref, tc.podman, tc.installed); got != tc.want {
				t.Fatalf("got %v, want %v", got, tc.want)
			}
		})
	}
}
