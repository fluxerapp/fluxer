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

// TestAuthWebAuthnMFALogin validates the WebAuthn MFA login flow where
// WebAuthn is used as a second factor after password authentication.
func TestAuthWebAuthnMFALogin(t *testing.T) {
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
		"name":       "MFA Passkey",
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

	loginReq := loginRequest{
		Email:    account.Email,
		Password: account.Password,
	}
	loginHTTPResp, err := client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to initiate login: %v", err)
	}
	if loginHTTPResp.StatusCode != http.StatusOK {
		t.Fatalf("login returned %d: %s", loginHTTPResp.StatusCode, readResponseBody(loginHTTPResp))
	}

	var loginResp loginResponse
	decodeJSONResponse(t, loginHTTPResp, &loginResp)

	if !loginResp.MFA {
		t.Fatal("expected MFA to be required")
	}
	if loginResp.Ticket == "" {
		t.Fatal("expected MFA ticket in login response")
	}
	if !loginResp.WebAuthn {
		t.Fatal("expected WebAuthn to be available as MFA method")
	}
	if loginResp.Token != "" {
		t.Fatal("expected no token before MFA completion")
	}

	// Request WebAuthn MFA authentication options
	var mfaOptions webAuthnAuthenticationOptions
	resp, err = client.postJSON("/auth/login/mfa/webauthn/authentication-options", map[string]string{
		"ticket": loginResp.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to request mfa webauthn options: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("mfa webauthn options returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	decodeJSONResponse(t, resp, &mfaOptions)

	if mfaOptions.Challenge == "" {
		t.Fatal("expected challenge in mfa authentication options")
	}
	if mfaOptions.RPID == "" {
		t.Fatal("expected RPID in mfa authentication options")
	}
	if mfaOptions.UserVerification != "required" {
		t.Fatalf("expected userVerification=required, got %q", mfaOptions.UserVerification)
	}
	if len(mfaOptions.AllowCredentials) == 0 {
		t.Fatal("expected allowed credentials in mfa authentication options")
	}

	if mfaOptions.RPID != "" {
		device.rpID = mfaOptions.RPID
	}

	mfaAssertion := device.authenticationResponse(t, mfaOptions)

	resp, err = client.postJSON("/auth/login/mfa/webauthn", map[string]any{
		"response":  mfaAssertion,
		"challenge": mfaOptions.Challenge,
		"ticket":    loginResp.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete webauthn mfa login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("webauthn mfa login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var webauthnMfaLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &webauthnMfaLogin)

	if webauthnMfaLogin.Token == "" {
		t.Fatal("expected token after successful MFA login")
	}

	resp, err = client.getWithAuth("/users/@me", webauthnMfaLogin.Token)
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
