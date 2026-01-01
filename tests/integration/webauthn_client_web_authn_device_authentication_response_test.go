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
	"crypto/ecdsa"
	"crypto/rand"
	"crypto/sha256"
	"testing"
)

func (d *webAuthnDevice) authenticationResponse(t testing.TB, options webAuthnAuthenticationOptions) map[string]any {
	t.Helper()

	challenge := decodeBase64URL(t, options.Challenge)
	clientData := map[string]any{
		"type":        "webauthn.get",
		"challenge":   encodeBase64URL(challenge),
		"origin":      d.origin,
		"crossOrigin": false,
	}
	clientDataJSON := mustJSON(t, clientData)

	authData := d.buildAssertionAuthData()
	clientDataHash := sha256.Sum256(clientDataJSON)
	sigInput := append(authData, clientDataHash[:]...)
	digest := sha256.Sum256(sigInput)

	signature, err := ecdsa.SignASN1(rand.Reader, d.privateKey, digest[:])
	if err != nil {
		t.Fatalf("failed to sign assertion: %v", err)
	}

	return map[string]any{
		"id":                     encodeBase64URL(d.credentialID),
		"rawId":                  encodeBase64URL(d.credentialID),
		"type":                   "public-key",
		"clientExtensionResults": map[string]any{},
		"response": map[string]any{
			"clientDataJSON":    encodeBase64URL(clientDataJSON),
			"authenticatorData": encodeBase64URL(authData),
			"signature":         encodeBase64URL(signature),
			"userHandle":        encodeBase64URL(d.userHandle),
		},
	}
}
