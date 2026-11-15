// SPDX-License-Identifier: AGPL-3.0-or-later

package secrets

import (
	"crypto/ecdh"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"strings"
)

type Kind int

const (
	KindHex32 Kind = iota
	KindBase64_32
	KindVAPID
	KindFixed
	KindRSA
	KindHex16
)

const Unset = "CHANGE_ME"

type Spec struct {
	Key      string
	Kind     Kind
	Fixed    string
	Stateful bool
}

const (
	VAPIDPublicKey  = "FLUXER_VAPID_PUBLIC_KEY"
	VAPIDPrivateKey = "FLUXER_VAPID_PRIVATE_KEY"
)

var Required = []Spec{
	{Key: "FLUXER_S3_ACCESS_KEY", Kind: KindFixed, Fixed: "fluxer"},
	{Key: "LIVEKIT_API_KEY", Kind: KindFixed, Fixed: "fluxer"},
	{Key: "POSTGRES_PASSWORD", Kind: KindHex32, Stateful: true},
	{Key: "MEILI_MASTER_KEY", Kind: KindHex32, Stateful: true},
	{Key: "FLUXER_S3_SECRET_KEY", Kind: KindHex32, Stateful: true},
	{Key: "FLUXER_SUDO_MODE_SECRET", Kind: KindHex32},
	{Key: "FLUXER_CONNECTION_INITIATION_SECRET", Kind: KindHex32},
	{Key: "FLUXER_PROFILE_PSEUDONYM_SECRET", Kind: KindHex32},
	{Key: "FLUXER_GATEWAY_RPC_AUTH_TOKEN", Kind: KindHex32},
	{Key: "FLUXER_ERLANG_COOKIE", Kind: KindHex32},
	{Key: "FLUXER_MEDIA_PROXY_SECRET_KEY", Kind: KindHex32},
	{Key: "FLUXER_ADMIN_SECRET_KEY_BASE", Kind: KindHex32},
	{Key: "FLUXER_ADMIN_OAUTH_CLIENT_SECRET", Kind: KindHex32},
	{Key: "LIVEKIT_API_SECRET", Kind: KindHex32, Stateful: true},
	{Key: "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64", Kind: KindBase64_32},
	{Key: "FLUXER_ACTIVITYPUB_SIGNING_KEY_BASE64", Kind: KindRSA},
	{Key: VAPIDPublicKey, Kind: KindVAPID, Stateful: true},
	{Key: VAPIDPrivateKey, Kind: KindVAPID, Stateful: true},
}

const (
	NATSAuthToken  = "FLUXER_NATS_AUTH_TOKEN"
	ValkeyPassword = "FLUXER_VALKEY_PASSWORD"
	SetupToken     = "FLUXER_SETUP_TOKEN"
)

var Native = []Spec{
	{Key: NATSAuthToken, Kind: KindHex32},
	{Key: ValkeyPassword, Kind: KindHex32},
	{Key: SetupToken, Kind: KindHex16},
}

var ErrStatefulMissing = errors.New("secrets: a key that guards stored data is missing")

type StatefulMissingError struct {
	Keys []string
}

func (e *StatefulMissingError) Error() string {
	return fmt.Sprintf("this instance has no value for %s and its existing data needs the old value", strings.Join(e.Keys, ", "))
}

func (e *StatefulMissingError) Unwrap() error { return ErrStatefulMissing }

func Hex(nBytes int) (string, error) {
	b := make([]byte, nBytes)
	if _, err := rand.Read(b); err != nil {
		return "", err
	}
	return hex.EncodeToString(b), nil
}

func Base64(nBytes int) (string, error) {
	b := make([]byte, nBytes)
	if _, err := rand.Read(b); err != nil {
		return "", err
	}
	return base64.StdEncoding.EncodeToString(b), nil
}

func RSAKey() (string, error) {
	key, err := rsa.GenerateKey(rand.Reader, 2048)
	if err != nil {
		return "", err
	}
	der, err := x509.MarshalPKCS8PrivateKey(key)
	if err != nil {
		return "", err
	}
	return base64.StdEncoding.EncodeToString(der), nil
}

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

func IsUnset(value string) bool {
	v := strings.TrimSpace(value)
	return v == "" || v == Unset
}

func Generate(s Spec) (string, error) {
	switch s.Kind {
	case KindHex32:
		return Hex(32)
	case KindHex16:
		return Hex(16)
	case KindBase64_32:
		return Base64(32)
	case KindRSA:
		return RSAKey()
	case KindFixed:
		return s.Fixed, nil
	}
	return "", fmt.Errorf("secrets: %s has no single-value generator", s.Key)
}

func Mint(values map[string]string, haveData bool) ([]string, error) {
	written := []string{}
	var blocked []string
	vapidMissing := false
	for _, s := range append(append([]Spec(nil), Required...), Native...) {
		if !IsUnset(values[s.Key]) {
			continue
		}
		if s.Stateful && haveData {
			blocked = append(blocked, s.Key)
			continue
		}
		if s.Kind == KindVAPID {
			vapidMissing = true
			continue
		}
		v, err := Generate(s)
		if err != nil {
			return written, err
		}
		values[s.Key] = v
		written = append(written, s.Key)
	}
	if vapidMissing {
		pair, err := GenerateVAPID()
		if err != nil {
			return written, err
		}
		values[VAPIDPublicKey] = pair.Public
		values[VAPIDPrivateKey] = pair.Private
		written = append(written, VAPIDPublicKey, VAPIDPrivateKey)
	}
	if len(blocked) > 0 {
		return written, &StatefulMissingError{Keys: blocked}
	}
	return written, nil
}
