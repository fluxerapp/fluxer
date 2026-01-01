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
	"testing"
)

func (d *webAuthnDevice) registerResponse(t testing.TB, options webAuthnRegistrationOptions) map[string]any {
	t.Helper()

	challenge := decodeBase64URL(t, options.Challenge)
	d.userHandle = decodeBase64URL(t, options.User.ID)

	if d.credentialID == nil {
		d.credentialID = randomBytes(t, 32)
	}

	clientData := map[string]any{
		"type":        "webauthn.create",
		"challenge":   encodeBase64URL(challenge),
		"origin":      d.origin,
		"crossOrigin": false,
	}
	clientDataJSON := mustJSON(t, clientData)

	authData := d.buildRegistrationAuthData(t)
	attestationObject := buildAttestationObject(t, authData)

	return map[string]any{
		"id":                     encodeBase64URL(d.credentialID),
		"rawId":                  encodeBase64URL(d.credentialID),
		"type":                   "public-key",
		"clientExtensionResults": map[string]any{},
		"response": map[string]any{
			"clientDataJSON":    encodeBase64URL(clientDataJSON),
			"attestationObject": encodeBase64URL(attestationObject),
			"transports":        []string{"internal"},
		},
	}
}
