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

func TestAuthMFASMSEnableDisable(t *testing.T) {
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
	resp.Body.Close()

	phone := fmt.Sprintf("+1555%07d", time.Now().UnixNano()%10000000)

	resp, err = client.postJSONWithAuth("/users/@me/phone/send-verification", map[string]string{
		"phone": phone,
	}, account.Token)
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

	loginReq := loginRequest{Email: account.Email, Password: account.Password}
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
		t.Fatalf("expected MFA to be required after enabling SMS MFA")
	}
	if !loginResp.SMS {
		t.Fatalf("expected SMS MFA to be enabled in login response")
	}
	if loginResp.Ticket == "" {
		t.Fatalf("expected ticket in login response when MFA is required")
	}
	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   totpCodeNow(t, secret),
		"ticket": loginResp.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete mfa login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("mfa login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var mfaLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &mfaLogin)
	account.Token = mfaLogin.Token

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

	loginHTTPResp, err = client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to initiate login after disabling SMS: %v", err)
	}
	if loginHTTPResp.StatusCode != http.StatusOK {
		t.Fatalf("login after disabling SMS returned %d: %s", loginHTTPResp.StatusCode, readResponseBody(loginHTTPResp))
	}

	decodeJSONResponse(t, loginHTTPResp, &loginResp)
	if loginResp.SMS {
		t.Fatalf("expected SMS MFA to be disabled in login response after disabling")
	}
	if !loginResp.MFA || !loginResp.TOTP {
		t.Fatalf("expected TOTP MFA to still be required after disabling SMS")
	}
}
