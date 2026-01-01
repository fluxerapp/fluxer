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

// TestAuthWebAuthnPasswordlessLogin validates passwordless login using WebAuthn
// with discoverable credentials (passkeys).
func TestAuthWebAuthnPasswordlessLogin(t *testing.T) {
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
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("enable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()
	var registrationOptions webAuthnRegistrationOptions
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to request registration options: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("registration options returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	decodeJSONResponse(t, resp, &registrationOptions)
	if registrationOptions.RP.ID != "" {
		device.rpID = registrationOptions.RP.ID
	}
	registrationResponse := device.registerResponse(t, registrationOptions)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials", map[string]any{
		"response":   registrationResponse,
		"challenge":  registrationOptions.Challenge,
		"name":       "Passwordless Passkey",
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to register credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("register credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()
	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
		"code":       totpCodeNow(t, secret),
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable totp: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("disable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	// Request discoverable WebAuthn authentication options (passwordless flow)
	var discoverableOptions webAuthnAuthenticationOptions
	resp, err = client.postJSON("/auth/webauthn/authentication-options", nil)
	if err != nil {
		t.Fatalf("failed to request discoverable webauthn options: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("discoverable webauthn options returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	decodeJSONResponse(t, resp, &discoverableOptions)

	if discoverableOptions.Challenge == "" {
		t.Fatal("expected challenge in authentication options")
	}
	if discoverableOptions.RPID == "" {
		t.Fatal("expected RPID in authentication options")
	}
	if discoverableOptions.UserVerification != "required" {
		t.Fatalf("expected userVerification=required, got %q", discoverableOptions.UserVerification)
	}

	if discoverableOptions.RPID != "" {
		device.rpID = discoverableOptions.RPID
	}

	discoverableAssertion := device.authenticationResponse(t, discoverableOptions)

	resp, err = client.postJSON("/auth/webauthn/authenticate", map[string]any{
		"response":  discoverableAssertion,
		"challenge": discoverableOptions.Challenge,
	})
	if err != nil {
		t.Fatalf("failed to complete passwordless webauthn login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("passwordless webauthn login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var passkeyLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &passkeyLogin)

	if passkeyLogin.Token == "" {
		t.Fatal("expected token in passwordless login response")
	}

	resp, err = client.getWithAuth("/users/@me", passkeyLogin.Token)
	if err != nil {
		t.Fatalf("failed to fetch user info with new token: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("fetch user info returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var userInfo userPrivateResponse
	decodeJSONResponse(t, resp, &userInfo)

	if userInfo.ID != account.UserID {
		t.Fatalf("user ID mismatch: expected %s, got %s", account.UserID, userInfo.ID)
	}
}
