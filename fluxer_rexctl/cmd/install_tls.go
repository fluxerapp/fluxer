// SPDX-License-Identifier: AGPL-3.0-or-later

package cmd

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/pem"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/checks"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/engine"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/instance"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/native"
	rt "github.com/fluxerapp/fluxer/fluxer_rexctl/internal/runtime"
	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/ui"
)

const (
	rootCertFile = "fluxer-root.crt"
	rootCertPath = "caddy/pki/authorities/local/root.crt"
)

type rootCert struct {
	path        string
	url         string
	fingerprint string
}

func localCA(inst *instance.Instance) bool {
	if isProxied(inst) {
		return false
	}
	return inst.State.Settings[edgeTLSVar] == "internal" || checks.PrivateDomain(inst.Domain())
}

func rootCertURL(inst *instance.Instance) string {
	host := inst.Domain()
	if p := inst.Setting("FLUXER_HTTP_PORT"); p != "" && p != "80" {
		host += ":" + p
	}
	return "http://" + host + "/" + rootCertFile
}

func saveRootCert(ctx context.Context, app *App, inst *instance.Instance, wait time.Duration) *rootCert {
	if !localCA(inst) {
		return nil
	}
	deadline := time.Now().Add(wait)
	for {
		data, err := readRootCert(ctx, app, inst)
		if block, _ := pem.Decode(data); err == nil && block != nil && block.Type == "CERTIFICATE" {
			out := inst.Path(rootCertFile)
			if old, err := os.ReadFile(out); err != nil || !bytes.Equal(old, data) {
				if os.WriteFile(out, data, 0o644) != nil {
					out = ""
				}
			}
			return &rootCert{path: out, url: rootCertURL(inst), fingerprint: fingerprint(block.Bytes)}
		}
		if time.Now().After(deadline) {
			return nil
		}
		select {
		case <-ctx.Done():
			return nil
		case <-time.After(2 * time.Second):
		}
	}
}

func fingerprint(der []byte) string {
	sum := sha256.Sum256(der)
	parts := make([]string, len(sum))
	for i, b := range sum {
		parts[i] = fmt.Sprintf("%02X", b)
	}
	return strings.Join(parts, ":")
}

func readRootCert(ctx context.Context, app *App, inst *instance.Instance) ([]byte, error) {
	switch rt.KindOf(inst) {
	case rt.Native:
		dir, _ := native.New(inst).DataDir("edge-data")
		return os.ReadFile(filepath.Join(dir, rootCertPath))
	case rt.Container:
		e, err := app.Engine(ctx, inst)
		if err != nil {
			return nil, err
		}
		return e.ReadSingleFile(ctx, rt.ContainerName(inst), rt.ContainerData+"/edge/"+rootCertPath)
	}
	p, err := app.Named(ctx, inst)
	if err != nil {
		return nil, err
	}
	var b, stderr bytes.Buffer
	if err := p.Exec(ctx, "edge", engine.ExecOptions{Stdout: &b, Stderr: &stderr}, "cat", "/data/"+rootCertPath); err != nil {
		return nil, err
	}
	return b.Bytes(), nil
}

func rootCertCard(u *ui.UI, cert *rootCert, origin string) {
	if cert == nil {
		return
	}
	rows := []ui.KV{{Key: "Download", Value: cert.url}, {Key: "SHA-256", Value: cert.fingerprint}}
	if cert.path != "" {
		rows = append(rows, ui.KV{Key: "File", Value: cert.path})
	}
	u.Card(ui.Card{
		Title: "Trust this instance on your devices",
		Rows:  rows,
		Body:  []string{fmt.Sprintf("Fluxer signs its own certificate for %s. On each computer and phone that opens it, download the root certificate, check that its SHA-256 fingerprint matches, install it as a trusted root, then restart the browser.", origin)},
		QR:    cert.url,
	})
}
