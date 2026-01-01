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

// TestAuthLoginMfaTotpFlagMatchesAuthenticatorTypes ensures MFA login responses
// only advertise TOTP when the authenticator types actually include it.
func TestAuthLoginMfaTotpFlagMatchesAuthenticatorTypes(t *testing.T) {
	t.Run("WebAuthnOnlyReportsTotpFalse", func(t *testing.T) {
		client := newTestClient(t)
		account := createTestAccount(t, client)
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

		loginWithTotp := loginTestUserWithTotp(t, client, account.Email, account.Password, secret)
		account.Token = loginWithTotp.Token

		device := newWebAuthnDevice(t)
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

		resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials", map[string]any{
			"response":   device.registerResponse(t, registrationOptions),
			"challenge":  registrationOptions.Challenge,
			"name":       "integration passkey",
			"mfa_method": "totp",
			"mfa_code":   totpCodeNow(t, secret),
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to register webauthn credential: %v", err)
		}
		if resp.StatusCode != http.StatusNoContent {
			t.Fatalf("register credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
			"code":       enableResp.BackupCodes[0].Code,
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

		loginResp, err := client.postJSON("/auth/login", loginRequest{
			Email:    account.Email,
			Password: account.Password,
		})
		if err != nil {
			t.Fatalf("failed to login: %v", err)
		}
		if loginResp.StatusCode != http.StatusOK {
			t.Fatalf("login returned %d: %s", loginResp.StatusCode, readResponseBody(loginResp))
		}
		var login loginResponse
		decodeJSONResponse(t, loginResp, &login)

		if !login.MFA {
			t.Fatalf("expected MFA to be required after totp disable")
		}
		if login.Ticket == "" {
			t.Fatalf("expected MFA ticket")
		}
		if !login.WebAuthn {
			t.Fatalf("expected WebAuthn to remain available")
		}
		if login.TOTP {
			t.Fatalf("expected TOTP flag to be false when totp is disabled")
		}
		if login.SMS {
			t.Fatalf("expected SMS flag to stay false for this user")
		}
	})

	t.Run("SmsOnlyReportsTotpFalse", func(t *testing.T) {
		client := newTestClient(t)
		account := createTestAccount(t, client)
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

		loginWithTotp := loginTestUserWithTotp(t, client, account.Email, account.Password, secret)
		account.Token = loginWithTotp.Token

		phone := fmt.Sprintf("+1555%07d", time.Now().UnixNano()%1_000_0000)
		resp, err = client.postJSONWithAuth("/users/@me/phone/send-verification", map[string]string{
			"phone": phone,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to send phone verification: %v", err)
		}
		if resp.StatusCode != http.StatusNoContent {
			t.Fatalf("send verification returned %d: %s", resp.StatusCode, readResponseBody(resp))
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
			t.Fatalf("enable sms returned %d: %s", resp.StatusCode, readResponseBody(resp))
		}
		resp.Body.Close()

		resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
			"code":       enableResp.BackupCodes[0].Code,
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

		loginResp, err := client.postJSON("/auth/login", loginRequest{
			Email:    account.Email,
			Password: account.Password,
		})
		if err != nil {
			t.Fatalf("failed to login: %v", err)
		}
		if loginResp.StatusCode != http.StatusOK {
			t.Fatalf("login returned %d: %s", loginResp.StatusCode, readResponseBody(loginResp))
		}
		var login loginResponse
		decodeJSONResponse(t, loginResp, &login)

		if login.MFA {
			t.Fatalf("expected MFA to be disabled once totp (and sms) are removed")
		}
		if login.Token == "" {
			t.Fatalf("expected session token after login")
		}
		if login.SMS {
			t.Fatalf("expected SMS flag to be false when sms mfa is implicitly removed")
		}
		if login.TOTP {
			t.Fatalf("expected TOTP flag to be false when totp is disabled")
		}
		if login.WebAuthn {
			t.Fatalf("expected WebAuthn flag to stay false in this scenario")
		}
	})
}
