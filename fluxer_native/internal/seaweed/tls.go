// SPDX-License-Identifier: AGPL-3.0-or-later

package seaweed

import (
	"context"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"fmt"
	"math/big"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/fluxerapp/fluxer/fluxer_native/internal/spec"
)

const (
	caFile       = "ca.crt"
	certFile     = "weed.crt"
	keyFile      = "weed.key"
	securityFile = "security.toml"
	certLifetime = 10 * 365 * 24 * time.Hour
)

var grpcComponents = []string{"master", "volume", "filer", "s3", "client"}

var httpsComponents = []string{"master", "volume", "filer"}

func ConfigDir(l spec.Layout) string { return filepath.Join(l.Runtime, "seaweedfs") }

func WriteTLS(dir string) error {
	if err := os.RemoveAll(dir); err != nil {
		return err
	}
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return err
	}
	caKey, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return err
	}
	now := time.Now().Add(-time.Hour)
	ca := &x509.Certificate{
		SerialNumber:          serial(),
		Subject:               pkix.Name{CommonName: "fluxerd seaweedfs ca"},
		NotBefore:             now,
		NotAfter:              now.Add(certLifetime),
		KeyUsage:              x509.KeyUsageCertSign,
		BasicConstraintsValid: true,
		IsCA:                  true,
	}
	caDER, err := x509.CreateCertificate(rand.Reader, ca, ca, &caKey.PublicKey, caKey)
	if err != nil {
		return err
	}
	ca, err = x509.ParseCertificate(caDER)
	if err != nil {
		return err
	}
	key, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return err
	}
	leaf := &x509.Certificate{
		SerialNumber: serial(),
		Subject:      pkix.Name{CommonName: "localhost"},
		DNSNames:     []string{"localhost"},
		IPAddresses:  []net.IP{net.ParseIP(spec.Loopback), net.IPv6loopback},
		NotBefore:    now,
		NotAfter:     now.Add(certLifetime),
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageServerAuth, x509.ExtKeyUsageClientAuth},
	}
	leafDER, err := x509.CreateCertificate(rand.Reader, leaf, ca, &key.PublicKey, caKey)
	if err != nil {
		return err
	}
	keyDER, err := x509.MarshalECPrivateKey(key)
	if err != nil {
		return err
	}
	files := map[string][]byte{
		caFile:       pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: caDER}),
		certFile:     pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: leafDER}),
		keyFile:      pem.EncodeToMemory(&pem.Block{Type: "EC PRIVATE KEY", Bytes: keyDER}),
		securityFile: securityTOML(dir),
	}
	for name, data := range files {
		if err := os.WriteFile(filepath.Join(dir, name), data, 0o600); err != nil {
			return err
		}
	}
	return nil
}

func serial() *big.Int {
	n, err := rand.Int(rand.Reader, new(big.Int).Lsh(big.NewInt(1), 127))
	if err != nil {
		panic(err)
	}
	return n
}

func securityTOML(dir string) []byte {
	ca, cert, key := filepath.Join(dir, caFile), filepath.Join(dir, certFile), filepath.Join(dir, keyFile)
	var b strings.Builder
	fmt.Fprintf(&b, "[grpc]\nca = %q\n", ca)
	for _, c := range grpcComponents {
		fmt.Fprintf(&b, "\n[grpc.%s]\ncert = %q\nkey = %q\n", c, cert, key)
	}
	fmt.Fprintf(&b, "\n[https.client]\nenabled = true\ncert = %q\nkey = %q\nca = %q\n", cert, key, ca)
	for _, c := range httpsComponents {
		fmt.Fprintf(&b, "\n[https.%s]\ncert = %q\nkey = %q\nca = %q\n", c, cert, key, ca)
	}
	return []byte(b.String())
}

func clientTLS(dir string) (*tls.Config, error) {
	pair, err := tls.LoadX509KeyPair(filepath.Join(dir, certFile), filepath.Join(dir, keyFile))
	if err != nil {
		return nil, err
	}
	pemCA, err := os.ReadFile(filepath.Join(dir, caFile))
	if err != nil {
		return nil, err
	}
	pool := x509.NewCertPool()
	if !pool.AppendCertsFromPEM(pemCA) {
		return nil, fmt.Errorf("seaweedfs: no certificate in %s", filepath.Join(dir, caFile))
	}
	return &tls.Config{Certificates: []tls.Certificate{pair}, RootCAs: pool, MinVersion: tls.VersionTLS12}, nil
}

func HTTPSProbe(dir string, port int, path string) spec.Probe {
	p := spec.FuncProbe(func(ctx context.Context) error {
		cfg, err := clientTLS(dir)
		if err != nil {
			return err
		}
		client := &http.Client{Transport: &http.Transport{Proxy: nil, DisableKeepAlives: true, TLSClientConfig: cfg}}
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, "https://"+spec.Addr(port)+path, nil)
		if err != nil {
			return err
		}
		resp, err := client.Do(req)
		if err != nil {
			return err
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			return fmt.Errorf("status %d", resp.StatusCode)
		}
		return nil
	})
	p.Name = "GET https://" + spec.Addr(port) + path
	return p
}
