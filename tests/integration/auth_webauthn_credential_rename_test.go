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

// TestAuthWebAuthnCredentialRename validates renaming a WebAuthn credential.
func TestAuthWebAuthnCredentialRename(t *testing.T) {
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
		"name":       "Original Name",
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

	resp, err = client.getWithAuth("/users/@me/mfa/webauthn/credentials", account.Token)
	if err != nil {
		t.Fatalf("failed to list credentials: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("list credentials returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	var credentials []webAuthnCredentialMetadata
	decodeJSONResponse(t, resp, &credentials)
	if len(credentials) != 1 {
		t.Fatalf("expected 1 credential, got %d", len(credentials))
	}
	credentialID := credentials[0].ID
	if credentials[0].Name != "Original Name" {
		t.Fatalf("expected credential name 'Original Name', got '%s'", credentials[0].Name)
	}
	resp, err = client.patchJSONWithAuth(fmt.Sprintf("/users/@me/mfa/webauthn/credentials/%s", credentialID), map[string]any{
		"name":       "Renamed Passkey",
		"mfa_method": "totp",
		"mfa_code":   totpCodeNow(t, secret),
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to rename credential: %v", err)
	}
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("rename credential returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	resp.Body.Close()

	resp, err = client.getWithAuth("/users/@me/mfa/webauthn/credentials", account.Token)
	if err != nil {
		t.Fatalf("failed to list credentials after rename: %v", err)
	}
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("list credentials returned %d: %s", resp.StatusCode, readResponseBody(resp))
	}
	decodeJSONResponse(t, resp, &credentials)
	if len(credentials) != 1 {
		t.Fatalf("expected 1 credential after rename, got %d", len(credentials))
	}
	if credentials[0].Name != "Renamed Passkey" {
		t.Fatalf("expected credential name 'Renamed Passkey', got '%s'", credentials[0].Name)
	}
	if credentials[0].ID != credentialID {
		t.Fatalf("credential ID changed after rename: expected %s, got %s", credentialID, credentials[0].ID)
	}
}
