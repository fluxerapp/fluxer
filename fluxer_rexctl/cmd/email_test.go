// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"strings"
	"testing"
)

func TestTLSFixPointsAtPrivateCA(t *testing.T) {
	for _, msg := range []string{"self-signed certificate", "unable to verify the first certificate", "self-signed certificate in certificate chain"} {
		got := tlsFix(msg, "")
		if !strings.Contains(got, "FLUXER_NODE_EXTRA_CA_CERTS") || !strings.Contains(got, "/etc/fluxer/ca/") || !strings.Contains(got, "ca folder") {
			t.Errorf("tlsFix(%q) = %q, want it to explain trusting a private CA with FLUXER_NODE_EXTRA_CA_CERTS", msg, got)
		}
	}
}
