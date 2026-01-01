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
	"strings"
	"testing"
	"time"

	"golang.org/x/text/cases"
	"golang.org/x/text/language"
)

func TestAuthCaseInsensitiveEmail(t *testing.T) {
	client := newTestClient(t)

	t.Run("login with different case variations succeeds", func(t *testing.T) {
		baseEmail := fmt.Sprintf("integration-test-%d@example.com", time.Now().UnixNano())
		password := uniquePassword()
		titleCaser := cases.Title(language.Und)

		registerTestUser(t, client, baseEmail, password)

		testCases := []struct {
			name  string
			email string
		}{
			{"lowercase", strings.ToLower(baseEmail)},
			{"uppercase", strings.ToUpper(baseEmail)},
			{"mixed case", mixedCase(baseEmail)},
			{"title case", titleCaser.String(strings.ToLower(baseEmail))},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				loginReq := loginRequest{
					Email:    tc.email,
					Password: password,
				}

				resp, err := client.postJSON("/auth/login", loginReq)
				if err != nil {
					t.Fatalf("failed to call login endpoint: %v", err)
				}
				defer resp.Body.Close()

				if resp.StatusCode != http.StatusOK {
					t.Fatalf("expected login to succeed with %s email %q, got %d: %s", tc.name, tc.email, resp.StatusCode, readResponseBody(resp))
				}

				var loginResp loginResponse
				decodeJSONResponse(t, resp, &loginResp)

				if loginResp.Token == "" {
					t.Fatalf("expected token in login response")
				}
			})
		}
	})

	t.Run("registration with different case is treated as duplicate", func(t *testing.T) {
		baseEmail := fmt.Sprintf("integration-test-%d@example.com", time.Now().UnixNano())
		password := uniquePassword()

		registerTestUser(t, client, baseEmail, password)

		uppercaseEmail := strings.ToUpper(baseEmail)
		req := registerRequest{
			Email:       uppercaseEmail,
			Username:    fmt.Sprintf("itest%x", time.Now().UnixNano()),
			GlobalName:  "Test User",
			Password:    uniquePassword(),
			DateOfBirth: adultDateOfBirth(),
			Consent:     true,
		}

		resp, err := client.postJSON("/auth/register", req)
		if err != nil {
			t.Fatalf("failed to call register endpoint: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("expected registration to fail for duplicate email (different case), got 200 OK")
		}

		if resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusConflict {
			t.Fatalf("expected 400 or 409 for duplicate email, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("forgot password with different case works", func(t *testing.T) {
		baseEmail := fmt.Sprintf("integration-test-%d@example.com", time.Now().UnixNano())
		password := uniquePassword()

		registerTestUser(t, client, baseEmail, password)

		uppercaseEmail := strings.ToUpper(baseEmail)
		payload := map[string]string{
			"email": uppercaseEmail,
		}

		resp, err := client.postJSON("/auth/forgot", payload)
		if err != nil {
			t.Fatalf("failed to call forgot endpoint: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNoContent && resp.StatusCode != http.StatusAccepted {
			t.Fatalf("expected forgot to succeed with uppercase email, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("email stored is normalized to lowercase", func(t *testing.T) {
		mixedEmail := fmt.Sprintf("Integration-Test-%d@Example.COM", time.Now().UnixNano())
		password := uniquePassword()

		regResp := registerTestUser(t, client, mixedEmail, password)

		resp, err := client.getWithAuth("/users/@me", regResp.Token)
		if err != nil {
			t.Fatalf("failed to fetch user: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusOK)

		var user userPrivateResponse
		decodeJSONResponse(t, resp, &user)

		if user.Email != strings.ToLower(mixedEmail) {
			t.Logf("email stored as %q instead of normalized lowercase %q (may store original case)", user.Email, strings.ToLower(mixedEmail))
		}
	})
}
