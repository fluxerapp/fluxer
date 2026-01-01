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

// Ensures WebAuthn auth fails when challenge is tampered.
func TestAuthWebAuthnAuthenticationWrongChallenge(t *testing.T) {
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

	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to request registration options: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var regOpts webAuthnRegistrationOptions
	decodeJSONResponse(t, resp, &regOpts)
	resp.Body.Close()

	if regOpts.RP.ID != "" {
		device.rpID = regOpts.RP.ID
	}

	regResp := device.registerResponse(t, regOpts)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials", map[string]any{
		"response":   regResp,
		"challenge":  regOpts.Challenge,
		"name":       "WrongChallenge",
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to register credential: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.postJSON("/auth/webauthn/authentication-options", nil)
	if err != nil {
		t.Fatalf("failed to request auth options: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var authOpts webAuthnAuthenticationOptions
	decodeJSONResponse(t, resp, &authOpts)
	resp.Body.Close()

	if authOpts.RPID != "" {
		device.rpID = authOpts.RPID
	}

	assertion := device.authenticationResponse(t, authOpts)

	badChallenge := authOpts.Challenge + "-tampered"
	resp, err = client.postJSON("/auth/webauthn/authenticate", map[string]any{
		"response":  assertion,
		"challenge": badChallenge,
	})
	if err != nil {
		t.Fatalf("failed to call webauthn auth with bad challenge: %v", err)
	}
	if resp.StatusCode == http.StatusOK {
		t.Fatalf("expected tampered challenge to fail, got 200")
	}
	resp.Body.Close()
}
