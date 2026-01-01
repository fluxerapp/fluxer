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

func TestAuthMFASMSLoginFlow(t *testing.T) {
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
		t.Fatalf("failed to initiate sms mfa login: %v", err)
	}
	if loginHTTPResp.StatusCode != http.StatusOK {
		t.Fatalf("login returned %d: %s", loginHTTPResp.StatusCode, readResponseBody(loginHTTPResp))
	}

	var loginResp loginResponse
	decodeJSONResponse(t, loginHTTPResp, &loginResp)
	if !loginResp.MFA {
		t.Fatalf("expected MFA to be required")
	}
	if !loginResp.SMS {
		t.Fatalf("expected SMS MFA to be available")
	}
	if loginResp.Ticket == "" {
		t.Fatalf("expected ticket in login response")
	}

	resp, err = client.postJSON("/auth/login/mfa/sms/send", map[string]string{
		"ticket": loginResp.Ticket,
	})
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
	if smsLogin.Token == "" {
		t.Fatalf("expected sms mfa login to return token")
	}

	resp, err = client.getWithAuth("/users/@me", smsLogin.Token)
	if err != nil {
		t.Fatalf("failed to fetch current user with sms mfa token: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("fetch current user returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var user struct {
		ID    string `json:"id"`
		Email string `json:"email"`
	}
	decodeJSONResponse(t, resp, &user)
	if user.ID != account.UserID {
		t.Fatalf("expected user id to be %s, got %s", account.UserID, user.ID)
	}
	if user.Email != account.Email {
		t.Fatalf("expected user email to be %s, got %s", account.Email, user.Email)
	}
	loginHTTPResp, err = client.postJSON("/auth/login", loginReq)
	if err != nil {
		t.Fatalf("failed to initiate second login: %v", err)
	}
	if loginHTTPResp.StatusCode != http.StatusOK {
		t.Fatalf("second login returned %d: %s", loginHTTPResp.StatusCode, readResponseBody(loginHTTPResp))
	}

	decodeJSONResponse(t, loginHTTPResp, &loginResp)
	if !loginResp.TOTP {
		t.Fatalf("expected TOTP to also be available as MFA method")
	}

	resp, err = client.postJSON("/auth/login/mfa/totp", map[string]string{
		"code":   totpCodeNow(t, secret),
		"ticket": loginResp.Ticket,
	})
	if err != nil {
		t.Fatalf("failed to complete totp mfa login: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("totp mfa login returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var totpLogin mfaLoginResponse
	decodeJSONResponse(t, resp, &totpLogin)
	if totpLogin.Token == "" {
		t.Fatalf("expected totp mfa login to return token")
	}
}
