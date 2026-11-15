// SPDX-License-Identifier: AGPL-3.0-or-later

package objectstore

import (
	"bytes"
	"context"
	"crypto/hmac"
	"crypto/rand"
	"crypto/sha256"
	"crypto/tls"
	"crypto/x509"
	"encoding/hex"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"slices"
	"strings"
	"time"
)

type Store struct {
	Endpoint  string
	Region    string
	AccessKey string
	SecretKey string
	PathStyle bool
}

type Problem int

const (
	Unreachable Problem = iota
	Untrusted
	WrongName
	Credentials
	WrongRegion
	MissingBuckets
	Denied
	Other
)

type Error struct {
	Problem Problem
	Buckets []string
	Code    string
	Message string
	Err     error
}

func (e *Error) Error() string {
	switch {
	case e.Err != nil:
		return e.Err.Error()
	case e.Problem == MissingBuckets && e.Code == "":
		return "missing buckets: " + strings.Join(e.Buckets, ", ")
	case e.Message != "":
		return e.Code + ": " + e.Message
	}
	return e.Code
}

func (e *Error) Unwrap() error { return e.Err }

func (s Store) Check(ctx context.Context, buckets []string) error {
	var missing []string
	for _, b := range buckets {
		err := s.probe(ctx, b)
		var se *Error
		if errors.As(err, &se) && se.Problem == MissingBuckets {
			missing = append(missing, b)
			continue
		}
		if err != nil {
			return err
		}
	}
	if len(missing) > 0 {
		return &Error{Problem: MissingBuckets, Buckets: missing}
	}
	return nil
}

func (s Store) probe(ctx context.Context, bucket string) error {
	var id [8]byte
	_, _ = rand.Read(id[:])
	key := "rexctl-check-" + hex.EncodeToString(id[:])
	if err := s.do(ctx, http.MethodPut, bucket, key, []byte("ok")); err != nil {
		return err
	}
	defer func() {
		ctx, cancel := context.WithTimeout(context.WithoutCancel(ctx), 10*time.Second)
		defer cancel()
		_ = s.do(ctx, http.MethodDelete, bucket, key, nil)
	}()
	if err := s.do(ctx, http.MethodGet, bucket, key, nil); err != nil {
		return err
	}
	return s.do(ctx, http.MethodDelete, bucket, key, nil)
}

type errorBody struct {
	Code    string `xml:"Code"`
	Message string `xml:"Message"`
}

func (s Store) do(ctx context.Context, method, bucket, key string, body []byte) error {
	req, err := s.request(ctx, method, bucket, key, body)
	if err != nil {
		return &Error{Problem: Unreachable, Err: err}
	}
	client := &http.Client{
		Timeout:       20 * time.Second,
		CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse },
	}
	resp, err := client.Do(req)
	if err != nil {
		return &Error{Problem: transportProblem(err), Err: err}
	}
	defer resp.Body.Close()
	data, _ := io.ReadAll(io.LimitReader(resp.Body, 64*1024))
	if resp.StatusCode < 300 {
		return nil
	}
	var eb errorBody
	_ = xml.Unmarshal(data, &eb)
	if eb.Code == "" {
		eb.Code = resp.Status
	}
	return &Error{Problem: classify(resp.StatusCode, eb.Code, eb.Message), Buckets: []string{bucket}, Code: eb.Code, Message: eb.Message}
}

var credentialCodes = []string{"InvalidAccessKeyId", "SignatureDoesNotMatch", "InvalidToken", "InvalidSecurity", "ExpiredToken", "TokenRefreshRequired", "Unauthorized", "InvalidSignature"}

var credentialMessages = []string{"invalid signature", "no such key", "access key", "signature does not match", "signature we calculated", "unauthorized"}

var regionCodes = []string{"AuthorizationHeaderMalformed", "PermanentRedirect", "IllegalLocationConstraintException", "InvalidRegion"}

func classify(status int, code, message string) Problem {
	msg := strings.ToLower(message)
	credentialText := slices.ContainsFunc(credentialMessages, func(m string) bool { return strings.Contains(msg, m) })
	switch {
	case code == "NoSuchBucket":
		return MissingBuckets
	case slices.Contains(credentialCodes, code), status == http.StatusUnauthorized, code == "InvalidArgument" && credentialText:
		return Credentials
	case slices.Contains(regionCodes, code), status == http.StatusMovedPermanently, status == http.StatusTemporaryRedirect:
		return WrongRegion
	case code == "AccessDenied" || status == http.StatusForbidden:
		if credentialText {
			return Credentials
		}
		return Denied
	case status == http.StatusNotFound && message == "":
		return MissingBuckets
	}
	return Other
}

func transportProblem(err error) Problem {
	var unknown x509.UnknownAuthorityError
	var invalid x509.CertificateInvalidError
	var host x509.HostnameError
	var verify *tls.CertificateVerificationError
	switch {
	case errors.As(err, &host):
		return WrongName
	case errors.As(err, &unknown), errors.As(err, &invalid), errors.As(err, &verify):
		return Untrusted
	}
	return Unreachable
}

func (s Store) request(ctx context.Context, method, bucket, key string, body []byte) (*http.Request, error) {
	u, err := url.Parse(s.Endpoint)
	if err != nil {
		return nil, err
	}
	base := strings.TrimSuffix(u.Path, "/")
	if s.PathStyle {
		u.Path = base + "/" + bucket + "/" + key
	} else {
		u.Host = bucket + "." + u.Host
		u.Path = base + "/" + key
	}
	u.RawPath, u.RawQuery, u.Fragment = "", "", ""
	req, err := http.NewRequestWithContext(ctx, method, u.String(), bytes.NewReader(body))
	if err != nil {
		return nil, err
	}
	s.sign(req, body, time.Now().UTC())
	return req, nil
}

func (s Store) sign(req *http.Request, body []byte, now time.Time) {
	payload := hashHex(body)
	stamp := now.Format("20060102T150405Z")
	day := now.Format("20060102")
	req.Header.Set("X-Amz-Date", stamp)
	req.Header.Set("X-Amz-Content-Sha256", payload)
	signed := "host;x-amz-content-sha256;x-amz-date"
	canonical := strings.Join([]string{
		req.Method,
		req.URL.EscapedPath(),
		"",
		"host:" + req.URL.Host,
		"x-amz-content-sha256:" + payload,
		"x-amz-date:" + stamp,
		"",
		signed,
		payload,
	}, "\n")
	scope := day + "/" + s.Region + "/s3/aws4_request"
	toSign := "AWS4-HMAC-SHA256\n" + stamp + "\n" + scope + "\n" + hashHex([]byte(canonical))
	k := hmacSHA256([]byte("AWS4"+s.SecretKey), day)
	k = hmacSHA256(k, s.Region)
	k = hmacSHA256(k, "s3")
	k = hmacSHA256(k, "aws4_request")
	sig := hex.EncodeToString(hmacSHA256(k, toSign))
	req.Header.Set("Authorization", fmt.Sprintf("AWS4-HMAC-SHA256 Credential=%s/%s, SignedHeaders=%s, Signature=%s", s.AccessKey, scope, signed, sig))
}

func hashHex(b []byte) string {
	h := sha256.Sum256(b)
	return hex.EncodeToString(h[:])
}

func hmacSHA256(key []byte, data string) []byte {
	m := hmac.New(sha256.New, key)
	m.Write([]byte(data))
	return m.Sum(nil)
}
