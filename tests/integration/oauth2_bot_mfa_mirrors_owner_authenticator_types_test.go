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
)

func TestOAuth2BotMfaMirrorsOwnerAuthenticatorTypes(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	_, firstBotID, firstBotToken := createOAuth2BotApplication(t, client, owner, "Pre-MFA Bot", []string{"https://example.com/callback"})
	firstBotUser := fetchUserMeWithToken(t, client, firstBotToken, true)
	if firstBotUser.ID != firstBotID {
		t.Fatalf("expected bot id %s, got %s", firstBotID, firstBotUser.ID)
	}
	if firstBotUser.MFAEnabled {
		t.Fatalf("expected bot to have MFA disabled before owner enables MFA")
	}
	if len(firstBotUser.AuthenticatorTypes) != 0 {
		t.Fatalf("expected bot authenticator_types to be empty before owner enables MFA, got %v", firstBotUser.AuthenticatorTypes)
	}

	secret := newTotpSecret(t)
	resp, err := client.postJSONWithAuth("/users/@me/mfa/totp/enable", map[string]string{
		"secret": secret,
		"code":   totpCodeNow(t, secret),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to enable totp: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("enable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	ownerMe := fetchUserMeWithToken(t, client, owner.Token, false)
	if !ownerMe.MFAEnabled {
		t.Fatalf("expected owner MFA to be enabled after enabling totp")
	}

	firstBotUser = fetchUserMeWithToken(t, client, firstBotToken, true)
	if !firstBotUser.MFAEnabled {
		t.Fatalf("expected bot to have MFA enabled after owner enables MFA")
	}
	if !containsAuthenticatorType(firstBotUser.AuthenticatorTypes, 0) {
		t.Fatalf("expected bot authenticator_types to include TOTP after owner enables it, got %v", firstBotUser.AuthenticatorTypes)
	}

	device := newWebAuthnDevice(t)

	var registrationOptions webAuthnRegistrationOptions
	resp, err = client.postJSONWithAuth("/users/@me/mfa/webauthn/credentials/registration-options", map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, owner.Token)
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
		"name":       "Bot Mirror Passkey",
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to register webauthn credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("register webauthn credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	ownerMe = fetchUserMeWithToken(t, client, owner.Token, false)
	if !containsAuthenticatorType(ownerMe.AuthenticatorTypes, 2) {
		t.Fatalf("expected owner authenticator_types to include WEBAUTHN after registration, got %v", ownerMe.AuthenticatorTypes)
	}

	firstBotUser = fetchUserMeWithToken(t, client, firstBotToken, true)
	if !containsAuthenticatorType(firstBotUser.AuthenticatorTypes, 2) {
		t.Fatalf("expected first bot authenticator_types to include WEBAUTHN after owner registers passkey, got %v", firstBotUser.AuthenticatorTypes)
	}

	_, secondBotID, secondBotToken := createOAuth2BotApplication(t, client, owner, "Post-MFA Bot", []string{"https://example.com/another"})
	secondBotUser := fetchUserMeWithToken(t, client, secondBotToken, true)
	if secondBotUser.ID != secondBotID {
		t.Fatalf("expected bot id %s, got %s", secondBotID, secondBotUser.ID)
	}
	if !secondBotUser.MFAEnabled {
		t.Fatalf("expected bot created after owner MFA enable to start with MFA enabled")
	}
	if !containsAuthenticatorType(secondBotUser.AuthenticatorTypes, 0) || !containsAuthenticatorType(secondBotUser.AuthenticatorTypes, 2) {
		t.Fatalf("expected new bot authenticator_types to include TOTP and WEBAUTHN, got %v", secondBotUser.AuthenticatorTypes)
	}

	resp, err = client.getWithAuth("/users/@me/mfa/webauthn/credentials", owner.Token)
	if err != nil {
		t.Fatalf("failed to list webauthn credentials: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("list webauthn credentials returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var credentials []webAuthnCredentialMetadata
	decodeJSONResponse(t, resp, &credentials)
	if len(credentials) == 0 {
		t.Fatal("expected at least one webauthn credential for owner")
	}
	credentialID := credentials[0].ID

	resp, err = client.deleteJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", credentialID), map[string]any{
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, owner.Token)
	if err != nil {
		t.Fatalf("failed to delete webauthn credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("delete webauthn credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	ownerMe = fetchUserMeWithToken(t, client, owner.Token, false)
	if containsAuthenticatorType(ownerMe.AuthenticatorTypes, 2) {
		t.Fatalf("expected owner authenticator_types to drop WEBAUTHN after deletion, got %v", ownerMe.AuthenticatorTypes)
	}
	firstBotUser = fetchUserMeWithToken(t, client, firstBotToken, true)
	if containsAuthenticatorType(firstBotUser.AuthenticatorTypes, 2) {
		t.Fatalf("expected first bot authenticator_types to drop WEBAUTHN after owner deletion, got %v", firstBotUser.AuthenticatorTypes)
	}
	secondBotUser = fetchUserMeWithToken(t, client, secondBotToken, true)
	if containsAuthenticatorType(secondBotUser.AuthenticatorTypes, 2) {
		t.Fatalf("expected second bot authenticator_types to drop WEBAUTHN after owner deletion, got %v", secondBotUser.AuthenticatorTypes)
	}

	disableCode := totpCodeNow(t, secret)
	disablePayload := map[string]any{
		"code":       disableCode,
		"mfa_method": "totp",
		"mfa_code":   disableCode,
	}
	resp, err = client.postJSONWithAuth("/users/@me/mfa/totp/disable", disablePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to disable totp: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("disable totp returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	ownerAfterDisable := fetchUserMeWithToken(t, client, owner.Token, false)
	if ownerAfterDisable.MFAEnabled {
		t.Fatalf("expected owner MFA to be disabled after disabling totp")
	}
	if len(ownerAfterDisable.AuthenticatorTypes) != 0 {
		t.Fatalf("expected owner authenticator_types to be empty after disabling totp, got %v", ownerAfterDisable.AuthenticatorTypes)
	}

	firstBotUser = fetchUserMeWithToken(t, client, firstBotToken, true)
	if firstBotUser.MFAEnabled {
		t.Fatalf("expected first bot MFA to be disabled after owner disables totp")
	}
	if len(firstBotUser.AuthenticatorTypes) != 0 {
		t.Fatalf("expected first bot authenticator_types cleared after owner disables totp, got %v", firstBotUser.AuthenticatorTypes)
	}

	_, thirdBotID, thirdBotToken := createOAuth2BotApplication(t, client, owner, "Post-MFA-Disable Bot", []string{"https://example.com/after-disable"})
	thirdBotUser := fetchUserMeWithToken(t, client, thirdBotToken, true)
	if thirdBotUser.ID != thirdBotID {
		t.Fatalf("expected bot id %s, got %s", thirdBotID, thirdBotUser.ID)
	}
	if thirdBotUser.MFAEnabled {
		t.Fatalf("expected new bot created after owner disabled MFA to start without MFA")
	}
	if len(thirdBotUser.AuthenticatorTypes) != 0 {
		t.Fatalf("expected new bot authenticator_types to be empty after owner disabled MFA, got %v", thirdBotUser.AuthenticatorTypes)
	}
}
