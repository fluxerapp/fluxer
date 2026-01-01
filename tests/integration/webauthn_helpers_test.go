/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

package integration

import (
	"bytes"
	"crypto/ecdsa"
	"crypto/rand"
	"crypto/sha256"
	"encoding/binary"
	"testing"
)

// buildAuthenticationWithoutUV constructs an authentication response where the UV flag is not set.
func buildAuthenticationWithoutUV(t testing.TB, device *webAuthnDevice, options webAuthnAuthenticationOptions) map[string]any {
	t.Helper()

	challenge := decodeBase64URL(t, options.Challenge)
	clientData := map[string]any{
		"type":        "webauthn.get",
		"challenge":   encodeBase64URL(challenge),
		"origin":      device.origin,
		"crossOrigin": false,
	}
	clientDataJSON := mustJSON(t, clientData)

	rpHash := sha256.Sum256([]byte(device.rpID))
	flags := byte(0x01)

	buf := &bytes.Buffer{}
	buf.Write(rpHash[:])
	buf.WriteByte(flags)

	device.signCount++
	if err := binary.Write(buf, binary.BigEndian, device.signCount); err != nil {
		t.Fatalf("failed to write sign count: %v", err)
	}

	authData := buf.Bytes()
	clientDataHash := sha256.Sum256(clientDataJSON)
	sigInput := append(authData, clientDataHash[:]...)
	digest := sha256.Sum256(sigInput)

	signature, err := ecdsa.SignASN1(rand.Reader, device.privateKey, digest[:])
	if err != nil {
		t.Fatalf("failed to sign assertion: %v", err)
	}

	return map[string]any{
		"id":                     encodeBase64URL(device.credentialID),
		"rawId":                  encodeBase64URL(device.credentialID),
		"type":                   "public-key",
		"clientExtensionResults": map[string]any{},
		"response": map[string]any{
			"clientDataJSON":    encodeBase64URL(clientDataJSON),
			"authenticatorData": encodeBase64URL(authData),
			"signature":         encodeBase64URL(signature),
			"userHandle":        encodeBase64URL(device.userHandle),
		},
	}
}
