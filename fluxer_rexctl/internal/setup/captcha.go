// SPDX-License-Identifier: AGPL-3.0-or-later

package setup

import (
	"bytes"
	"context"
	"crypto/pbkdf2"
	"crypto/sha256"
	"encoding/base64"
	"encoding/binary"
	"encoding/hex"
	"encoding/json"
	"errors"
	"math"
	"net/http"
	"runtime"
	"sync"
	"sync/atomic"

	"github.com/fluxerapp/fluxer/fluxer_rexctl/internal/adminapi"
)

const (
	captchaHeader    = "X-Captcha-Token"
	altchaAlgorithm  = "PBKDF2/SHA-256"
	maxCaptchaRounds = 3
)

var ErrCaptcha = errors.New("setup: couldn't solve the sign-up challenge")

type altchaChallenge struct {
	Parameters struct {
		Algorithm string `json:"algorithm"`
		Nonce     string `json:"nonce"`
		Salt      string `json:"salt"`
		Cost      int    `json:"cost"`
		KeyLength int    `json:"keyLength"`
		KeyPrefix string `json:"keyPrefix"`
	} `json:"parameters"`
}

type altchaSolution struct {
	Counter    uint32 `json:"counter"`
	DerivedKey string `json:"derivedKey"`
}

func register(ctx context.Context, c *adminapi.Client, account map[string]any, out any) error {
	var headers map[string]string
	for range maxCaptchaRounds {
		err := c.Loopback(ctx, http.MethodPost, "/auth/register", headers, account, out)
		raw, ok := captchaChallenge(err)
		if !ok {
			return err
		}
		token, serr := solveAltcha(ctx, raw)
		if serr != nil {
			return serr
		}
		headers = map[string]string{captchaHeader: token}
	}
	return ErrCaptcha
}

func captchaChallenge(err error) (json.RawMessage, bool) {
	var apiErr *adminapi.APIError
	if !errors.As(err, &apiErr) || (apiErr.Code != "CAPTCHA_REQUIRED" && apiErr.Code != "INVALID_CAPTCHA") {
		return nil, false
	}
	var body struct {
		Provider  string          `json:"captcha_provider"`
		Challenge json.RawMessage `json:"altcha_challenge"`
	}
	if json.Unmarshal(apiErr.Body, &body) != nil || body.Provider != "altcha" || len(body.Challenge) == 0 {
		return nil, false
	}
	return body.Challenge, true
}

func solveAltcha(ctx context.Context, raw json.RawMessage) (string, error) {
	var ch altchaChallenge
	if err := json.Unmarshal(raw, &ch); err != nil {
		return "", err
	}
	p := ch.Parameters
	nonce, nerr := hex.DecodeString(p.Nonce)
	salt, serr := hex.DecodeString(p.Salt)
	prefix, perr := hex.DecodeString(p.KeyPrefix)
	if p.Algorithm != altchaAlgorithm || nerr != nil || serr != nil || perr != nil || p.Cost <= 0 || p.KeyLength <= 0 || len(prefix) == 0 {
		return "", ErrCaptcha
	}
	sol, err := searchAltcha(ctx, nonce, salt, prefix, p.Cost, p.KeyLength)
	if err != nil {
		return "", err
	}
	payload, err := json.Marshal(struct {
		Challenge json.RawMessage `json:"challenge"`
		Solution  altchaSolution  `json:"solution"`
	}{raw, *sol})
	if err != nil {
		return "", err
	}
	return base64.StdEncoding.EncodeToString(payload), nil
}

func searchAltcha(parent context.Context, nonce, salt, prefix []byte, cost, keyLength int) (*altchaSolution, error) {
	ctx, cancel := context.WithCancel(parent)
	defer cancel()
	workers := runtime.NumCPU()
	var next atomic.Uint64
	var once sync.Once
	var found *altchaSolution
	var wg sync.WaitGroup
	for range workers {
		wg.Go(func() {
			password := make([]byte, len(nonce)+4)
			copy(password, nonce)
			for ctx.Err() == nil {
				n := next.Add(1) - 1
				if n > math.MaxUint32 {
					return
				}
				binary.BigEndian.PutUint32(password[len(nonce):], uint32(n))
				key, err := pbkdf2.Key(sha256.New, string(password), salt, cost, keyLength)
				if err != nil {
					return
				}
				if bytes.HasPrefix(key, prefix) {
					once.Do(func() {
						found = &altchaSolution{Counter: uint32(n), DerivedKey: hex.EncodeToString(key)}
						cancel()
					})
					return
				}
			}
		})
	}
	wg.Wait()
	if found != nil {
		return found, nil
	}
	if err := parent.Err(); err != nil {
		return nil, err
	}
	return nil, ErrCaptcha
}
