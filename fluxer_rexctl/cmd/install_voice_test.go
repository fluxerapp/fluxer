// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import "testing"

func TestPickVoicePairDefaultFree(t *testing.T) {
	tcp, udp := pickVoicePair(func(int, string) bool { return true })
	if tcp != 7881 || udp != 7882 {
		t.Errorf("all free = %d/%d, want 7881/7882", tcp, udp)
	}
}

func TestPickVoicePairStepsByTwo(t *testing.T) {
	busy := map[int]bool{7881: true, 7882: true}
	tcp, udp := pickVoicePair(func(n int, _ string) bool { return !busy[n] })
	if tcp != 7883 || udp != 7884 {
		t.Errorf("default pair busy = %d/%d, want 7883/7884", tcp, udp)
	}
	busy[7883], busy[7884] = true, true
	tcp, udp = pickVoicePair(func(n int, _ string) bool { return !busy[n] })
	if tcp != 7885 || udp != 7886 {
		t.Errorf("two pairs busy = %d/%d, want 7885/7886", tcp, udp)
	}
}

func TestPickVoicePairNeedsBothSides(t *testing.T) {
	tcp, udp := pickVoicePair(func(n int, proto string) bool {
		if proto == "udp" && n == 7882 {
			return false
		}
		return true
	})
	if tcp != 7883 || udp != 7884 {
		t.Errorf("udp side of default busy = %d/%d, want 7883/7884", tcp, udp)
	}
}
