// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"crypto/sha256"
	"encoding/hex"
	"encoding/pem"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func fakeRexctlArchive(t *testing.T, argsFile string) []byte {
	t.Helper()
	script := fmt.Sprintf("#!/bin/sh\nfor a in \"$@\"; do printf '%%s\\n' \"$a\"; done > %q\n", argsFile)
	var buf bytes.Buffer
	gz := gzip.NewWriter(&buf)
	tw := tar.NewWriter(gz)
	if err := tw.WriteHeader(&tar.Header{Name: "rexctl", Mode: 0o755, Size: int64(len(script))}); err != nil {
		t.Fatal(err)
	}
	if _, err := tw.Write([]byte(script)); err != nil {
		t.Fatal(err)
	}
	if err := tw.Close(); err != nil {
		t.Fatal(err)
	}
	if err := gz.Close(); err != nil {
		t.Fatal(err)
	}
	return buf.Bytes()
}

func TestInstallScriptMapsVoiceShorthand(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("install.sh runs on Linux and macOS")
	}
	for _, tool := range []string{"sh", "curl", "tar"} {
		if _, err := exec.LookPath(tool); err != nil {
			t.Skipf("%s is missing", tool)
		}
	}
	script, err := filepath.Abs("../../fluxer_docs/src/installer/install.sh")
	if err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		args []string
		want []string
	}{
		{[]string{"voice", "chat.example.com", "fvj_1_x"}, []string{"voice", "join", "chat.example.com", "fvj_1_x"}},
		{[]string{"voice", "chat.example.com", "fvj_1_x", "--yes"}, []string{"voice", "join", "chat.example.com", "fvj_1_x", "--yes"}},
		{[]string{"voice", "chat.example.com"}, []string{"voice", "join", "chat.example.com"}},
		{[]string{"voice", "chat.example.com", "--yes"}, []string{"voice", "join", "chat.example.com", "--yes"}},
		{[]string{"voice", "join", "chat.example.com", "fvj_1_x"}, []string{"voice", "join", "chat.example.com", "fvj_1_x"}},
		{[]string{"voice", "--help"}, []string{"voice", "--help"}},
		{[]string{"voice", "list"}, []string{"voice", "list"}},
		{[]string{"voice", "remove", "fra-server-1"}, []string{"voice", "remove", "fra-server-1"}},
		{[]string{"voice", "token", "list"}, []string{"voice", "token", "list"}},
		{[]string{"voice", "token", "revoke", "123"}, []string{"voice", "token", "revoke", "123"}},
		{[]string{"voice", "hostname", "fra-server-1", "voice.example.com"}, []string{"voice", "hostname", "fra-server-1", "voice.example.com"}},
		{[]string{"voice", "status", "fra-server-1", "--yes"}, []string{"voice", "status", "fra-server-1", "--yes"}},
	}
	for _, tc := range cases {
		t.Run(strings.Join(tc.args, " "), func(t *testing.T) {
			dir := t.TempDir()
			argsFile := filepath.Join(dir, "args")
			archive := fakeRexctlArchive(t, argsFile)
			sum := sha256.Sum256(archive)
			var srv *httptest.Server
			srv = httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if strings.HasSuffix(r.URL.Path, ".tar.gz") {
					_, _ = w.Write(archive)
					return
				}
				fmt.Fprintf(w, "version v9.9.9\nurl %s/rexctl.tar.gz\nsha256 %s\n", srv.URL, hex.EncodeToString(sum[:]))
			}))
			defer srv.Close()
			ca := filepath.Join(dir, "ca.pem")
			certPEM := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: srv.Certificate().Raw})
			if err := os.WriteFile(ca, certPEM, 0o644); err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command("sh", append([]string{script}, tc.args...)...)
			cmd.Env = []string{
				"PATH=" + os.Getenv("PATH"),
				"HOME=" + dir,
				"REXCTL_RELEASE_URL=" + srv.URL + "/release.json",
				"CURL_CA_BUNDLE=" + ca,
				"SSL_CERT_FILE=" + ca,
				"FLUXER_VOICE_JOIN_TOKEN=fvj_1_x",
			}
			cmd.Stdin = strings.NewReader("")
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("install.sh: %v\n%s", err, out)
			}
			got, err := os.ReadFile(argsFile)
			if err != nil {
				t.Fatalf("rexctl never ran: %v\n%s", err, out)
			}
			if strings.TrimSpace(string(got)) != strings.Join(tc.want, "\n") {
				t.Fatalf("rexctl got %q, want %q", strings.Fields(string(got)), tc.want)
			}
		})
	}
}
