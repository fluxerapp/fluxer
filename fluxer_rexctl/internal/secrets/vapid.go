// SPDX-License-Identifier: AGPL-3.0-or-later

package secrets

import (
	"bytes"
	"crypto/ecdh"
	"crypto/rand"
	"encoding/base64"
	"errors"
	"strings"
)

const (
	VAPIDPublicKey  = "FLUXER_VAPID_PUBLIC_KEY"
	VAPIDPrivateKey = "FLUXER_VAPID_PRIVATE_KEY"
)

var (
	ErrVAPIDMismatch = errors.New("vapid: public key does not match private key")
	ErrVAPIDMissing  = errors.New("vapid: no key pair")
	ErrVAPIDInvalid  = errors.New("vapid: key has the wrong length or encoding")
)

type VAPID struct {
	Public  string
	Private string
}

func GenerateVAPID() (VAPID, error) {
	key, err := ecdh.P256().GenerateKey(rand.Reader)
	if err != nil {
		return VAPID{}, err
	}
	return VAPID{
		Public:  base64.RawURLEncoding.EncodeToString(key.PublicKey().Bytes()),
		Private: base64.RawURLEncoding.EncodeToString(key.Bytes()),
	}, nil
}

func ValidateVAPID(public, private string) error {
	public, private = NormalizeBase64URL(public), NormalizeBase64URL(private)
	if public == "" && private == "" {
		return ErrVAPIDMissing
	}
	pub, err := base64.RawURLEncoding.DecodeString(public)
	if err != nil || len(pub) != 65 || pub[0] != 4 {
		return ErrVAPIDInvalid
	}
	if _, err := ecdh.P256().NewPublicKey(pub); err != nil {
		return ErrVAPIDInvalid
	}
	priv, err := base64.RawURLEncoding.DecodeString(private)
	if err != nil || len(priv) != 32 {
		return ErrVAPIDInvalid
	}
	key, err := ecdh.P256().NewPrivateKey(priv)
	if err != nil {
		return ErrVAPIDInvalid
	}
	if !bytes.Equal(key.PublicKey().Bytes(), pub) {
		return ErrVAPIDMismatch
	}
	return nil
}

func NormalizeBase64URL(s string) string {
	s = strings.TrimSpace(s)
	s = strings.NewReplacer("+", "-", "/", "_", "=", "").Replace(s)
	return s
}
