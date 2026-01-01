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

func TestAuthPhoneVerificationFlow(t *testing.T) {
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

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch current user: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("fetch current user returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	var user struct {
		Phone *string `json:"phone"`
	}
	decodeJSONResponse(t, resp, &user)
	if user.Phone == nil || *user.Phone != phone {
		got := ""
		if user.Phone != nil {
			got = *user.Phone
		}
		t.Fatalf("expected user phone to be %s, got %s", phone, got)
	}

	resp, err = client.deleteJSONWithAuth("/users/@me/phone", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to remove phone: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("remove phone returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch current user after removal: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("fetch current user after removal returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}

	decodeJSONResponse(t, resp, &user)
	if user.Phone != nil && *user.Phone != "" {
		t.Fatalf("expected user phone to be empty after removal, got %s", *user.Phone)
	}
}
