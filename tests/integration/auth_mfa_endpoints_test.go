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
	"fmt"
	"net/http"
	"testing"
	"time"
)

func TestAuthMFAEndpoints(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	device := newWebAuthnDevice(t)
	var passkeyID string

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
	if len(enableResp.BackupCodes) == 0 {
		t.Fatalf("expected backup codes after enabling totp")
	}

	resp, err = client.postJSONWithAuth("/users/@me/mfa/backup-codes", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
		"regenerate": false,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to fetch backup codes: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("fetch backup codes returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var fetched backupCodesResponse
	decodeJSONResponse(t, resp, &fetched)
	if len(fetched.BackupCodes) != len(enableResp.BackupCodes) {
		t.Fatalf("expected %d backup codes, got %d", len(enableResp.BackupCodes), len(fetched.BackupCodes))
	}

	loginReq := loginRequest{Email: account.Email, Password: account.Password}
	loginHTTPResp, err := client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to initiate login for totp: %v", err)
	}
	if loginHTTPResp.StatusCode != http.StatusOK {
		t.Fatalf("login returned %d: %s", loginHTTPResp.StatusCode, readResponseBody(loginHTTPResp))
	}
	var loginResp loginResponse
	decodeJSONResponse(t, loginHTTPResp, &loginResp)
	if !loginResp.MFA || loginResp.Ticket == "" || !loginResp.TOTP {
		t.Fatalf("expected login to require totp mfa")
	}

	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   enableResp.BackupCodes[0].Code,
		"ticket": loginResp.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete mfa totp login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("mfa totp login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var totpLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &totpLogin)
	if totpLogin.Token == "" {
		t.Fatalf("expected mfa login to return token")
	}
	account.Token = totpLogin.Token

	resp, err = client.postJSONWithAuth("/users/@me/mfa/backup-codes", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
		"regenerate": true,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to regenerate backup codes: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("regenerate backup codes returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var regenerated backupCodesResponse
	decodeJSONResponse(t, resp, &regenerated)
	if len(regenerated.BackupCodes) == 0 {
		t.Fatalf("expected regenerated backup codes")
	}

	phone := fmt.Sprintf("+1555%07d", time.Now().UnixNano()%1_000_0000)
	resp, err = client.postJSONWithAuth("/users/@me/phone/send-verification", map[string]string{"phone": phone}, account.Token)
	if err != nil {
		t.Fatalf("failed to send phone verification: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("send phone verification returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/phone/verify", map[string]string{
		"phone": phone,
		"code":  "123456",
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to verify phone: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("verify phone returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var phoneVerify phoneVerifyResponse
	decodeJSONResponse(t, resp, &phoneVerify)
	if phoneVerify.PhoneToken == "" {
		t.Fatalf("expected phone verify to return token")
	}

	resp, err = client.postJSONWithAuth("/users/@me/phone", map[string]any{
		"phone_token": phoneVerify.PhoneToken,
		"mfa_method":  "totp",
		"mfa_code":    totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to attach phone: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("attach phone returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/mfa/sms/enable", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to enable sms mfa: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("enable sms mfa returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	loginHTTPResp, err = client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to initiate sms mfa login: %v", err)
	}
	if loginHTTPResp.StatusCode != http.StatusOK {
		t.Fatalf("login returned %d: %s", loginHTTPResp.StatusCode, readResponseBody(loginHTTPResp))
	}
	decodeJSONResponse(t, loginHTTPResp, &loginResp)
	if !loginResp.MFA || !loginResp.SMS || loginResp.Ticket == "" {
		t.Fatalf("expected sms mfa requirements in login response")
	}

	resp, err = client.postJSON("/auth/login/mfa/sms/send", map[string]string{"ticket": loginResp.Ticket})
	if err != nil {
		t.Fatalf("failed to send sms mfa code: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("sms send returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.postJSON("/auth/login/mfa/sms", map[string]string{
		"ticket": loginResp.Ticket,
		"code":   "123456",
	})
	if err != nil {
		t.Fatalf("failed to complete sms mfa login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("sms mfa login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var smsLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &smsLogin)
	account.Token = smsLogin.Token

	resp, err = client.postJSONWithAuth("/users/@me/mfa/sms/disable", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable sms mfa: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("disable sms mfa returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.deleteJSONWithAuth("/users/@me/phone", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNext(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to remove phone: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("remove phone returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	var registrationOptions webAuthnRegistrationOptions
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
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
	t.Logf("registration options: rp_id=%s challenge=%s", registrationOptions.RP.ID, registrationOptions.Challenge)

	registrationResponse := device.registerResponse(t, registrationOptions)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials", map[string]any{
		"response":   registrationResponse,
		"challenge":  registrationOptions.Challenge,
		"name":       "Integration Passkey",
		"mfa_method": "totp",
		"mfa_code":   totpCodeNext(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to register webauthn credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("register webauthn credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/mfa/webauthn/credentials", account.Token)
	if err != nil {
		t.Fatalf("failed to list webauthn credentials: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("list webauthn credentials returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var credentials []webAuthnCredentialMetadata
	decodeJSONResponse(t, resp, &credentials)
	if len(credentials) != 1 {
		t.Fatalf("expected one credential, got %d", len(credentials))
	}
	passkeyID = credentials[0].ID
	if passkeyID != encodeBase64URL(device.credentialID) {
		t.Fatalf("credential id mismatch: server=%s device=%s", passkeyID, encodeBase64URL(device.credentialID))
	}
	t.Logf("registered webauthn credential id=%s", passkeyID)

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", passkeyID), map[string]any{
		"name":       "Renamed Integration Passkey",
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to rename webauthn credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("rename webauthn credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	loginHTTPResp, err = client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to initiate webauthn mfa login: %v", err)
	}
	if loginHTTPResp.StatusCode != http.StatusOK {
		t.Fatalf("login returned %d: %s", loginHTTPResp.StatusCode, readResponseBody(loginHTTPResp))
	}
	decodeJSONResponse(t, loginHTTPResp, &loginResp)
	if !loginResp.MFA || loginResp.Ticket == "" {
		t.Fatalf("expected login to require mfa before webauthn assertion")
	}

	var mfaOptions webAuthnAuthenticationOptions
	resp, err = client.postJSON("/auth/login/mfa/webauthn/authentication-options", map[string]string{
		"ticket": loginResp.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to request webauthn mfa options: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("mfa webauthn options returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	decodeJSONResponse(t, resp, &mfaOptions)
	if mfaOptions.RPID != "" {
		device.rpID = mfaOptions.RPID
	}
	t.Logf("mfa webauthn options: rp_id=%s challenge=%s allow=%d", mfaOptions.RPID, mfaOptions.Challenge, len(mfaOptions.AllowCredentials))
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
		t.Fatalf("mfa webauthn login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var webauthnMfaLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &webauthnMfaLogin)
	account.Token = webauthnMfaLogin.Token

	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
		"code":       regenerated.BackupCodes[0].Code,
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

	// Get WebAuthn sudo challenge to delete the credential
	var sudoWebAuthnOptions webAuthnAuthenticationOptions
	resp, err = client.postJSONWithAuth("/users/@me/sudo/webauthn/authentication-options", map[string]any{}, account.Token)
	if err != nil {
		t.Fatalf("failed to get sudo webauthn options: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("sudo webauthn options returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	decodeJSONResponse(t, resp, &sudoWebAuthnOptions)
	if sudoWebAuthnOptions.RPID != "" {
		device.rpID = sudoWebAuthnOptions.RPID
	}
	sudoAssertion := device.authenticationResponse(t, sudoWebAuthnOptions)

	resp, err = client.deleteJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", passkeyID), map[string]any{
		"mfa_method":         "webauthn",
		"webauthn_response":  sudoAssertion,
		"webauthn_challenge": sudoWebAuthnOptions.Challenge,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to delete webauthn credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("delete webauthn credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	clearTestEmails(t, client)

	// Use an explicit different IP for the attacker to ensure IP authorization is tested
	// even when FLUXER_TEST_IP environment variable is set
	attackerIP := "10.99.88.77"
	if attackerIP == client.clientIP {
		attackerIP = "10.99.88.78"
	}
	attacker := &testClient{
		baseURL:    client.baseURL,
		httpClient: client.httpClient,
		clientIP:   attackerIP,
	}
	attackResp, err := attacker.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to attempt login from new ip: %v", err)
	}
	if attackResp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected new ip login to fail with 403, got %d: %s", attackResp.StatusCode, readResponseBody(attackResp))
	}

	email := waitForEmail(t, client, "ip_authorization", account.Email)
	token, ok := email.Metadata["token"]
	if !ok || token == "" {
		t.Fatalf("expected ip authorization email token")
	}

	resp, err = attacker.postJSON("/auth/authorize-ip", map[string]string{"token": token})
	if err != nil {
		t.Fatalf("failed to authorize ip: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("authorize ip returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	loginResult := loginTestUser(t, attacker, account.Email, account.Password)
	if loginResult.MFA {
		t.Fatalf("did not expect mfa after authorizing ip")
	}
	account.Token = loginResult.Token
}
