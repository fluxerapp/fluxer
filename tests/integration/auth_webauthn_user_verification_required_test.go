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
	"net/http"
	"testing"
)

// Ensures WebAuthn authentication requires user verification (UV flag set).
func TestAuthWebAuthnUserVerificationRequired(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	device := newWebAuthnDevice(t)

	secret := newTotpSecret(t)
	resp, err := client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret,
		"code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to enable totp: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()
	var registrationOptions webAuthnRegistrationOptions
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to request registration options: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &registrationOptions)
	resp.Body.Close()

	if registrationOptions.RP.ID != "" {
		device.rpID = registrationOptions.RP.ID
	}
	registrationResponse := device.registerResponse(t, registrationOptions)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials", map[string]any{
		"response":   registrationResponse,
		"challenge":  registrationOptions.Challenge,
		"name":       "UV required",
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to register credential: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	// Get passwordless authentication options.
	var authOptions webAuthnAuthenticationOptions
	resp, err = client.postJSON("/auth/webauthn/authentication-options", nil)
	if err != nil {
		t.Fatalf("failed to request authentication options: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &authOptions)
	resp.Body.Close()

	if authOptions.RPID != "" {
		device.rpID = authOptions.RPID
	}

	assertion := buildAuthenticationWithoutUV(t, device, authOptions)
	resp, err = client.postJSON("/auth/webauthn/authenticate", map[string]any{
		"response":  assertion,
		"challenge": authOptions.Challenge,
	})
	if err != nil {
		t.Fatalf("failed to post authentication: %v", err)
	}
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("expected UV-less assertion to be rejected, got 200")
	}
	resp.Body.Close()
}
