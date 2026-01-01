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

type mfaMethodsResponse struct {
	TOTP     bool `json:"totp"`
	SMS      bool `json:"sms"`
	WebAuthn bool `json:"webauthn"`
	HasMFA   bool `json:"has_mfa"`
}

func TestAuthSudoMFAMethods(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)

	resp, err := client.getWithAuth("/users/@me/sudo/mfa-methods", account.Token)
	if err != nil {
		t.Fatalf("failed to get mfa methods: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var methods mfaMethodsResponse
	decodeJSONResponse(t, resp, &methods)
	if methods.HasMFA {
		t.Fatalf("expected has_mfa=false for user without MFA, got true")
	}
	if methods.TOTP || methods.SMS || methods.WebAuthn {
		t.Fatalf("expected all MFA methods to be false, got totp=%v sms=%v webauthn=%v", methods.TOTP, methods.SMS, methods.WebAuthn)
	}

	secret := newTotpSecret(t)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret,
		"code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to enable totp: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var enableResp backupCodesResponse
	decodeJSONResponse(t, resp, &enableResp)
	if len(enableResp.BackupCodes) == 0 {
		t.Fatalf("expected backup codes after enabling totp")
	}

	account.loginWithTotp(t, client, secret)

	resp, err = client.getWithAuth("/users/@me/sudo/mfa-methods", account.Token)
	if err != nil {
		t.Fatalf("failed to get mfa methods after enabling totp: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	decodeJSONResponse(t, resp, &methods)
	if !methods.HasMFA {
		t.Fatalf("expected has_mfa=true after enabling totp")
	}
	if !methods.TOTP {
		t.Fatalf("expected totp=true after enabling totp")
	}
	if methods.SMS || methods.WebAuthn {
		t.Fatalf("expected sms and webauthn to be false, got sms=%v webauthn=%v", methods.SMS, methods.WebAuthn)
	}
}
