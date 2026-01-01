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

func TestSudoModeSensitiveEndpointsRequireVerification(t *testing.T) {
	client := newTestClient(t)
	account := createTestAccount(t, client)
	device := newWebAuthnDevice(t)

	phone := fmt.Sprintf("+1555%07d", time.Now().UnixNano()%1_000_0000)
	resp, err := client.postJSONWithAuth("/users/@me/phone/send-verification", map[string]string{
		"phone": phone,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to send phone verification: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/phone/verify", map[string]string{
		"phone": phone,
		"code":  "123456",
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to verify phone: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var phoneVerify phoneVerifyResponse
	decodeJSONResponse(t, resp, &phoneVerify)

	resp, err = client.postJSONWithAuth("/users/@me/phone", map[string]string{
		"phone_token": phoneVerify.PhoneToken,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt phone attach without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.postJSONWithAuth("/users/@me/phone", map[string]string{
		"phone_token": phoneVerify.PhoneToken,
		"password":    account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to attach phone with password: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	secret := newTotpSecret(t)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret":   secret,
		"code":     totpCodeNow(t, secret),
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to enable totp: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/mfa/sms/enable", nil, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt sms enable without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.postJSONWithAuth("/users/@me/mfa/sms/enable", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNext(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to enable sms mfa with totp: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/mfa/sms/disable", nil, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt sms disable without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.postJSONWithAuth("/users/@me/mfa/sms/disable", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable sms mfa with totp: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	disableTotpCode := totpCodeNow(t, secret)
	sudoTotpCode := totpCodePrev(t, secret)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", map[string]any{
		"code":       disableTotpCode,
		"mfa_method": "totp",
		"mfa_code":   sudoTotpCode,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to disable totp: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.deleteJSONWithAuth("/users/@me/phone", nil, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt phone removal without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.deleteJSONWithAuth("/users/@me/phone", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to remove phone with password: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	guild := createGuild(t, client, account.Token, "Sudo Guild Delete Test")
	guildID := parseSnowflake(t, guild.ID)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/delete", guildID), map[string]any{}, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt guild delete without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/delete", guildID), map[string]any{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to delete guild with password: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	newOwner := createTestAccount(t, client)
	transferGuild := createGuild(t, client, account.Token, "Sudo Guild Transfer Test")
	transferGuildID := parseSnowflake(t, transferGuild.ID)
	systemChannelID := parseSnowflake(t, transferGuild.SystemChannel)
	invite := createChannelInvite(t, client, account.Token, systemChannelID)
	joinGuild(t, client, newOwner.Token, invite.Code)

	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/guilds/%d/transfer-ownership", transferGuildID),
		map[string]string{
			"new_owner_id": newOwner.UserID,
		},
		account.Token,
	)
	if err != nil {
		t.Fatalf("failed to attempt guild transfer without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.postJSONWithAuth(
		fmt.Sprintf("/guilds/%d/transfer-ownership", transferGuildID),
		map[string]string{
			"new_owner_id": newOwner.UserID,
			"password":     account.Password,
		},
		account.Token,
	)
	if err != nil {
		t.Fatalf("failed to transfer guild ownership with password: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	resp.Body.Close()

	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", nil, account.Token)
	if err != nil {
		t.Fatalf("failed to request webauthn options without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", map[string]string{
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to request webauthn options with password: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var registrationOptions webAuthnRegistrationOptions
	decodeJSONResponse(t, resp, &registrationOptions)
	if registrationOptions.RP.ID != "" {
		device.rpID = registrationOptions.RP.ID
	}

	registrationResponse := device.registerResponse(t, registrationOptions)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials", map[string]any{
		"response":  registrationResponse,
		"challenge": registrationOptions.Challenge,
		"name":      "Sudo Mode Test Passkey",
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt webauthn registration without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials", map[string]any{
		"response":  registrationResponse,
		"challenge": registrationOptions.Challenge,
		"name":      "Sudo Mode Test Passkey",
		"password":  account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to register webauthn credential with password: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/mfa/webauthn/credentials", account.Token)
	if err != nil {
		t.Fatalf("failed to list webauthn credentials: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var credentials []webAuthnCredentialMetadata
	decodeJSONResponse(t, resp, &credentials)
	if len(credentials) != 1 {
		t.Fatalf("expected a single credential, got %d", len(credentials))
	}
	credentialID := credentials[0].ID

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", credentialID), map[string]string{
		"name": "Attempted Rename Without Password",
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt passkey rename without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	secret2 := newTotpSecret(t)
	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret":   secret2,
		"code":     totpCodeNow(t, secret2),
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to re-enable totp: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)
	var enableTotpResp struct {
		BackupCodes []struct {
			Code string `json:"code"`
		} `json:"backup_codes"`
	}
	decodeJSONResponse(t, resp, &enableTotpResp)
	if len(enableTotpResp.BackupCodes) < 2 {
		t.Fatalf("expected at least 2 backup codes, got %d", len(enableTotpResp.BackupCodes))
	}
	resp.Body.Close()

	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", credentialID), map[string]any{
		"name":       "Renamed With TOTP",
		"mfa_method": "totp",
		"mfa_code":   enableTotpResp.BackupCodes[0].Code,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to rename passkey with totp: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	resp, err = client.deleteJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", credentialID), nil, account.Token)
	if err != nil {
		t.Fatalf("failed to attempt passkey deletion without sudo: %v", err)
	}
	assertSudoModeRequired(t, resp)

	resp, err = client.deleteJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", credentialID), map[string]any{
		"mfa_method": "totp",
		"mfa_code":   enableTotpResp.BackupCodes[1].Code,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to delete passkey with totp: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()
}
