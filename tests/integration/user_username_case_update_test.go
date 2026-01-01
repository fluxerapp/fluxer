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
	"strings"
	"testing"
)

func TestUserUsernameCaseUpdate(t *testing.T) {
	runCaseUpdateTest(t, "re-sending_existing_username_keeps_discriminator", func(t *testing.T, client *testClient, account testAccount, initialUser userPartial) {
		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"username": initialUser.Username,
			"password": account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to submit no-op username update: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var updatedUser userPrivateResponse
		decodeJSONResponse(t, resp, &updatedUser)

		if updatedUser.Username != initialUser.Username {
			t.Fatalf("expected username to remain %q, got %q", initialUser.Username, updatedUser.Username)
		}

		if updatedUser.Discriminator != initialUser.Discriminator {
			t.Fatalf("expected discriminator to remain %s, got %s", initialUser.Discriminator, updatedUser.Discriminator)
		}
	})

	runCaseUpdateTest(t, "changing_username_case_preserves_discriminator", func(t *testing.T, client *testClient, account testAccount, initialUser userPartial) {
		newUsername := strings.ToUpper(initialUser.Username)
		if newUsername == initialUser.Username {
			newUsername = strings.ToLower(initialUser.Username)
		}

		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"username": newUsername,
			"password": account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to update username: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var updatedUser userPrivateResponse
		decodeJSONResponse(t, resp, &updatedUser)

		if updatedUser.Username != newUsername {
			t.Fatalf("expected username to be %q, got %q", newUsername, updatedUser.Username)
		}

		if updatedUser.Discriminator != initialUser.Discriminator {
			t.Fatalf("expected discriminator to remain %s, got %s", initialUser.Discriminator, updatedUser.Discriminator)
		}
	})

	runCaseUpdateTest(t, "changing_username_completely_works", func(t *testing.T, client *testClient, account testAccount, initialUser userPartial) {
		newUsername := "diff" + initialUser.Username[:min(len(initialUser.Username), 28)]

		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"username": newUsername,
			"password": account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to update username: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var updatedUser userPrivateResponse
		decodeJSONResponse(t, resp, &updatedUser)

		if updatedUser.Username != newUsername {
			t.Fatalf("expected username to be %q, got %q", newUsername, updatedUser.Username)
		}
	})

	runCaseUpdateTest(t, "no-op_username_and_discriminator_stays_unchanged", func(t *testing.T, client *testClient, account testAccount, initialUser userPartial) {
		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      initialUser.Username,
			"discriminator": initialUser.Discriminator,
			"password":      account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to submit no-op update: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var updatedUser userPrivateResponse
		decodeJSONResponse(t, resp, &updatedUser)

		if updatedUser.Username != initialUser.Username {
			t.Fatalf("expected username to remain %q, got %q", initialUser.Username, updatedUser.Username)
		}
		if updatedUser.Discriminator != initialUser.Discriminator {
			t.Fatalf("expected discriminator to remain %s, got %s", initialUser.Discriminator, updatedUser.Discriminator)
		}
	})

	runCaseUpdateTest(t, "case-only_change_with_explicit_discriminator_keeps_discriminator", func(t *testing.T, client *testClient, account testAccount, initialUser userPartial) {
		newUsername := strings.ToLower(initialUser.Username)
		if newUsername == initialUser.Username {
			newUsername = strings.ToUpper(initialUser.Username)
		}

		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      newUsername,
			"discriminator": initialUser.Discriminator,
			"password":      account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to update username: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var updatedUser userPrivateResponse
		decodeJSONResponse(t, resp, &updatedUser)

		if updatedUser.Username != newUsername {
			t.Fatalf("expected username to be %q, got %q", newUsername, updatedUser.Username)
		}
		if updatedUser.Discriminator != initialUser.Discriminator {
			t.Fatalf("expected discriminator to remain %s, got %s", initialUser.Discriminator, updatedUser.Discriminator)
		}
	})

	runCaseUpdateTest(t, "non-premium_username_change_rerolls_even_if_same_discriminator_sent", func(t *testing.T, client *testClient, account testAccount, initialUser userPartial) {
		newUsername := "reroll" + initialUser.Username[:min(len(initialUser.Username), 26)]

		resp, err := client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      newUsername,
			"discriminator": initialUser.Discriminator,
			"password":      account.Password,
		}, account.Token)
		if err != nil {
			t.Fatalf("failed to update username: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var updatedUser userPrivateResponse
		decodeJSONResponse(t, resp, &updatedUser)

		if updatedUser.Username != newUsername {
			t.Fatalf("expected username to be %q, got %q", newUsername, updatedUser.Username)
		}
		if updatedUser.Discriminator == initialUser.Discriminator {
			t.Fatalf("expected discriminator to change when username changed for non-premium; still %s", updatedUser.Discriminator)
		}
	})
}

func runCaseUpdateTest(t *testing.T, name string, test func(t *testing.T, client *testClient, account testAccount, initialUser userPartial)) {
	t.Helper()
	t.Run(name, func(t *testing.T) {
		client := newTestClient(t)
		account := createTestAccount(t, client)

		resp, err := client.getWithAuth("/users/@me", account.Token)
		if err != nil {
			t.Fatalf("failed to fetch user: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var initialUser userPartial
		decodeJSONResponse(t, resp, &initialUser)

		test(t, client, account, initialUser)
	})
}
