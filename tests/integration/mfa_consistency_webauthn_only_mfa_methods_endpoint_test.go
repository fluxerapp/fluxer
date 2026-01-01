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

// TestMfaConsistencyWebAuthnOnlyMfaMethodsEndpoint verifies that the
// /users/@me/sudo/mfa-methods endpoint correctly reports has_mfa=true
// for users with only WebAuthn (no TOTP).
func TestMfaConsistencyWebAuthnOnlyMfaMethodsEndpoint(t *testing.T) {
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
	var enableResp backupCodesResponse
	decodeJSONResponse(t, resp, &enableResp)

	loginResp, err := client.postJSON("/auth/login", loginRequest{
		Email:    account.Email,
		Password: account.Password,
	})
	if err != nil {
		t.Fatalf("failed to login: %v", err)
	}
	var login loginResponse
	decodeJSONResponse(t, loginResp, &login)

	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   enableResp.BackupCodes[0].Code,
		"ticket": login.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete MFA login: %v", err)
	}
	var mfaLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &mfaLogin)
	account.Token = mfaLogin.Token

	// Register a WebAuthn credential
	var registrationOptions webAuthnRegistrationOptions
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   enableResp.BackupCodes[1].Code,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to request webauthn registration options: %v", err)
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
		"name":       "Test Passkey",
		"mfa_method": "totp",
		"mfa_code":   enableResp.BackupCodes[2].Code,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to register webauthn credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("register webauthn credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
		"code":       enableResp.BackupCodes[3].Code,
		"mfa_method": "totp",
		"mfa_code":   enableResp.BackupCodes[4].Code,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable totp: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("disable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	// Login via passkey to get a fresh token
	var discoverableOptions webAuthnAuthenticationOptions
	resp, err = client.postJSON("/auth/webauthn/authentication-options", nil)
	if err != nil {
		t.Fatalf("failed to request discoverable webauthn options: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("discoverable webauthn options returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	decodeJSONResponse(t, resp, &discoverableOptions)
	if discoverableOptions.RPID != "" {
		device.rpID = discoverableOptions.RPID
	}

	discoverableAssertion := device.authenticationResponse(t, discoverableOptions)
	resp, err = client.postJSON("/auth/webauthn/authenticate", map[string]any{
		"response":  discoverableAssertion,
		"challenge": discoverableOptions.Challenge,
	})
	if err != nil {
		t.Fatalf("failed to complete discoverable webauthn login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("discoverable webauthn login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var passkeyLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &passkeyLogin)
	account.Token = passkeyLogin.Token

	resp, err = client.getWithAuth("/users/@me/sudo/mfa-methods", account.Token)
	if err != nil {
		t.Fatalf("failed to get mfa methods: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("mfa methods returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var mfaMethods struct {
		TOTP     bool `json:"totp"`
		SMS      bool `json:"sms"`
		WebAuthn bool `json:"webauthn"`
		HasMFA   bool `json:"has_mfa"`
	}
	decodeJSONResponse(t, resp, &mfaMethods)

	if !mfaMethods.HasMFA {
		t.Fatalf("expected has_mfa=true for WebAuthn-only user, got false")
	}
	if mfaMethods.TOTP {
		t.Fatalf("expected totp=false for WebAuthn-only user, got true")
	}
	if !mfaMethods.WebAuthn {
		t.Fatalf("expected webauthn=true for WebAuthn-only user, got false")
	}

	t.Logf("correctly reported has_mfa=true for WebAuthn-only user")
}
