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

func TestAuthRegisterValidation(t *testing.T) {
	client := newTestClient(t)

	t.Run("invalid email format is rejected", func(t *testing.T) {
		req := registerRequest{
			Email:       "not-an-email",
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
			t.Fatalf("expected registration to fail with invalid email, got 200 OK")
		}
		if resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected 400 Bad Request for invalid email, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("weak password is rejected", func(t *testing.T) {
		email := fmt.Sprintf("integration-test-%d@example.com", time.Now().UnixNano())
		req := registerRequest{
			Email:       email,
			Username:    fmt.Sprintf("itest%x", time.Now().UnixNano()),
			GlobalName:  "Test User",
			Password:    "weak",
			DateOfBirth: adultDateOfBirth(),
			Consent:     true,
		}

		resp, err := client.postJSON("/auth/register", req)
		if err != nil {
			t.Fatalf("failed to call register endpoint: %v", err)
		}
		defer resp.Body.Close()

		if resp.StatusCode == http.StatusOK {
			t.Fatalf("expected registration to fail with weak password, got 200 OK")
		}
		if resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected 400 Bad Request for weak password, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("duplicate email is rejected", func(t *testing.T) {
		account := createTestAccount(t, client)

		req := registerRequest{
			Email:       account.Email,
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
			t.Fatalf("expected registration to fail with duplicate email, got 200 OK")
		}
		if resp.StatusCode != http.StatusBadRequest && resp.StatusCode != http.StatusConflict {
			t.Fatalf("expected 400 Bad Request or 409 Conflict for duplicate email, got %d: %s", resp.StatusCode, readResponseBody(resp))
		}
	})

	t.Run("missing required fields are rejected", func(t *testing.T) {
		testCases := []struct {
			name string
			req  registerRequest
		}{
			{
				name: "missing email",
				req: registerRequest{
					Username:    fmt.Sprintf("itest%x", time.Now().UnixNano()),
					GlobalName:  "Test User",
					Password:    uniquePassword(),
					DateOfBirth: adultDateOfBirth(),
					Consent:     true,
				},
			},
			{
				name: "missing username",
				req: registerRequest{
					Email:       fmt.Sprintf("integration-test-%d@example.com", time.Now().UnixNano()),
					GlobalName:  "Test User",
					Password:    uniquePassword(),
					DateOfBirth: adultDateOfBirth(),
					Consent:     true,
				},
			},
			{
				name: "missing password",
				req: registerRequest{
					Email:       fmt.Sprintf("integration-test-%d@example.com", time.Now().UnixNano()),
					Username:    fmt.Sprintf("itest%x", time.Now().UnixNano()),
					GlobalName:  "Test User",
					DateOfBirth: adultDateOfBirth(),
					Consent:     true,
				},
			},
			{
				name: "missing date of birth",
				req: registerRequest{
					Email:      fmt.Sprintf("integration-test-%d@example.com", time.Now().UnixNano()),
					Username:   fmt.Sprintf("itest%x", time.Now().UnixNano()),
					GlobalName: "Test User",
					Password:   uniquePassword(),
					Consent:    true,
				},
			},
		}

		for _, tc := range testCases {
			t.Run(tc.name, func(t *testing.T) {
				resp, err := client.postJSON("/auth/register", tc.req)
				if err != nil {
					t.Fatalf("failed to call register endpoint: %v", err)
				}
				defer resp.Body.Close()

				if resp.StatusCode == http.StatusOK {
					t.Fatalf("expected registration to fail for %s, got 200 OK", tc.name)
				}
				if resp.StatusCode != http.StatusBadRequest {
					t.Fatalf("expected 400 Bad Request for %s, got %d: %s", tc.name, resp.StatusCode, readResponseBody(resp))
				}
			})
		}
	})
}
