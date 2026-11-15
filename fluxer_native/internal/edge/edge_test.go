// SPDX-License-Identifier: AGPL-3.0-or-later

package edge

import (
	"io"
	"net"
	"net/http"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

func freePort(t *testing.T) int {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	return ln.Addr().(*net.TCPAddr).Port
}

func testConfig(t *testing.T) (Config, string) {
	t.Helper()
	port := freePort(t)
	return Config{
		Site:           ":" + strconv.Itoa(port),
		Bind:           spec.Loopback,
		HTTPPort:       freePort(t),
		HTTPSPort:      freePort(t),
		TrustedProxies: "private_ranges",
		Encode:         "zstd gzip",
		Protocols:      "h1 h2",
		AdminBasePath:  "/admin",
		Dir:            t.TempDir(),
		CaddyDir:       filepath.Join(t.TempDir(), "caddy.d"),
	}, "http://127.0.0.1:" + strconv.Itoa(port)
}

func get(t *testing.T, url string) (int, string) {
	t.Helper()
	client := &http.Client{Timeout: 5 * time.Second, Transport: &http.Transport{Proxy: nil}}
	resp, err := client.Get(url)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	return resp.StatusCode, string(body)
}

func TestRouting(t *testing.T) {
	c, base := testConfig(t)
	ln, err := net.Listen("tcp", spec.Addr(spec.PortAppProxy))
	if err != nil {
		t.Skipf("%s is in use: %v", spec.Addr(spec.PortAppProxy), err)
	}
	srv := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) { io.WriteString(w, "spa") })}
	go srv.Serve(ln)
	t.Cleanup(func() { srv.Close() })
	if err := Load(c, true); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { Stop() })
	cases := map[string]string{"/.env": "404", "/.git/config": "404", "/web/.hidden": "404",
		"/channels/@me": "200 spa", "/.well-known/fluxer/x": "200 spa"}
	if listenUpstream(t, spec.Addr(spec.PortAPI)) != nil {
		cases["/api/v1/thing"] = "200 /v1/thing 127.0.0.1"
		cases["/.well-known/fluxer"] = "200 /.well-known/fluxer 127.0.0.1"
	}
	if listenUpstream(t, spec.Addr(spec.PortMediaProxy)) != nil {
		cases["/media/attachments/1/2/.gitignore"] = "200 /attachments/1/2/.gitignore 127.0.0.1"
		cases["/media/external/s/https/raw.githubusercontent.com/o/r/main/.github/logo.png"] = "200 /external/s/https/raw.githubusercontent.com/o/r/main/.github/logo.png 127.0.0.1"
	}
	for path, want := range cases {
		code, body := get(t, base+path)
		got := strconv.Itoa(code)
		if code == http.StatusOK {
			got += " " + body
		}
		if got != want {
			t.Errorf("%s returned %s, want %s", path, got, want)
		}
	}
}

func listenUpstream(t *testing.T, addr string) *http.Server {
	t.Helper()
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		return nil
	}
	srv := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, r.URL.Path+" "+r.Header.Get("X-Real-IP"))
	})}
	go srv.Serve(ln)
	t.Cleanup(func() { srv.Close() })
	return srv
}
