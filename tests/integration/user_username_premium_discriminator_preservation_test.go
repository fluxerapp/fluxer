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

type premiumUsernameScenario struct {
	client              *testClient
	account             testAccount
	finalUser           userPrivateResponse
	customDiscriminator string
	baseUsername        string
}

func setupPremiumUsernameScenario(t *testing.T) premiumUsernameScenario {
	t.Helper()

	client := newTestClient(t)
	account := createTestAccount(t, client)
	grantSubscriptionPremium(t, client, account.UserID)

	resp, err := client.getWithAuth("/users/@me", account.Token)
	if err != nil {
		t.Fatalf("failed to fetch user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var initialUser userPartial
	decodeJSONResponse(t, resp, &initialUser)

	customDiscriminator := "4242"
	if customDiscriminator == initialUser.Discriminator {
		customDiscriminator = "4243"
	}

	resp, err = client.patchJSONWithAuth("/users/@me", map[string]any{
		"discriminator": customDiscriminator,
		"password":      account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to set custom discriminator: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var updatedUser userPrivateResponse
	decodeJSONResponse(t, resp, &updatedUser)
	if updatedUser.Discriminator != customDiscriminator {
		t.Fatalf("expected discriminator to be set to %s, got %s", customDiscriminator, updatedUser.Discriminator)
	}

	newUsername := initialUser.Username + "p"
	resp, err = client.patchJSONWithAuth("/users/@me", map[string]any{
		"username": newUsername,
		"password": account.Password,
	}, account.Token)
	if err != nil {
		t.Fatalf("failed to change username for premium user: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var finalUser userPrivateResponse
	decodeJSONResponse(t, resp, &finalUser)

	return premiumUsernameScenario{
		client:              client,
		account:             account,
		finalUser:           finalUser,
		customDiscriminator: customDiscriminator,
		baseUsername:        initialUser.Username,
	}
}

func TestPremiumUsernameChangeKeepsCustomDiscriminator(t *testing.T) {
	t.Run("subscription_premium_keeps_custom_discriminator_on_username_change", func(t *testing.T) {
		scenario := setupPremiumUsernameScenario(t)

		if scenario.finalUser.Username != scenario.baseUsername+"p" {
			t.Fatalf("expected username to be %q, got %q", scenario.baseUsername+"p", scenario.finalUser.Username)
		}

		if scenario.finalUser.Discriminator != scenario.customDiscriminator {
			t.Fatalf("expected discriminator to remain %s after username change, got %s", scenario.customDiscriminator, scenario.finalUser.Discriminator)
		}
	})

	t.Run("subscription_premium_can_request_new_discriminator_on_username_change", func(t *testing.T) {
		scenario := setupPremiumUsernameScenario(t)
		newerUsername := scenario.finalUser.Username + "x"
		requestedDiscriminator := "5151"

		resp, err := scenario.client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      newerUsername,
			"discriminator": requestedDiscriminator,
			"password":      scenario.account.Password,
		}, scenario.account.Token)
		if err != nil {
			t.Fatalf("failed to change username with requested discriminator: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var newerUser userPrivateResponse
		decodeJSONResponse(t, resp, &newerUser)

		if newerUser.Username != newerUsername {
			t.Fatalf("expected username to be %q, got %q", newerUsername, newerUser.Username)
		}

		if newerUser.Discriminator != requestedDiscriminator {
			t.Fatalf("expected discriminator to be updated to %s, got %s", requestedDiscriminator, newerUser.Discriminator)
		}
	})

	t.Run("subscription_premium_keeps_discriminator_when_requesting_same_tag", func(t *testing.T) {
		scenario := setupPremiumUsernameScenario(t)

		resp, err := scenario.client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      scenario.finalUser.Username,
			"discriminator": scenario.finalUser.Discriminator,
			"password":      scenario.account.Password,
		}, scenario.account.Token)
		if err != nil {
			t.Fatalf("failed to submit no-op premium update: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var sameUser userPrivateResponse
		decodeJSONResponse(t, resp, &sameUser)

		if sameUser.Discriminator != scenario.finalUser.Discriminator {
			t.Fatalf("expected discriminator to remain %s, got %s", scenario.finalUser.Discriminator, sameUser.Discriminator)
		}
	})

	t.Run("subscription_premium_cannot_take_an_existing_tag_for_username", func(t *testing.T) {
		scenario := setupPremiumUsernameScenario(t)
		other := createTestAccount(t, scenario.client)
		grantSubscriptionPremium(t, scenario.client, other.UserID)

		clashUsername := fmt.Sprintf("clash%d", time.Now().UnixNano())
		clashDiscriminator := "7777"

		resp, err := scenario.client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      clashUsername,
			"discriminator": clashDiscriminator,
			"password":      other.Password,
		}, other.Token)
		if err != nil {
			t.Fatalf("failed to set clash user: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		resp, err = scenario.client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      clashUsername,
			"discriminator": clashDiscriminator,
			"password":      scenario.account.Password,
		}, scenario.account.Token)
		if err != nil {
			t.Fatalf("failed to attempt conflicting tag: %v", err)
		}
		if resp.StatusCode != http.StatusBadRequest {
			t.Fatalf("expected 400 when tag is taken, got %d", resp.StatusCode)
		}
	})

	t.Run("lifetime_premium_can_request_0000_discriminator_on_username_change", func(t *testing.T) {
		scenario := setupPremiumUsernameScenario(t)
		grantPremium(t, scenario.client, scenario.account.UserID, PremiumTypeLifetime)

		newerUsername := scenario.finalUser.Username + "l"
		resp, err := scenario.client.patchJSONWithAuth("/users/@me", map[string]any{
			"username":      newerUsername,
			"discriminator": "0000",
			"password":      scenario.account.Password,
		}, scenario.account.Token)
		if err != nil {
			t.Fatalf("failed to request 0000 discriminator: %v", err)
		}
		assertStatus(t, resp, http.StatusOK)

		var lifetimeUser userPrivateResponse
		decodeJSONResponse(t, resp, &lifetimeUser)

		if lifetimeUser.Username != newerUsername {
			t.Fatalf("expected username to be %q, got %q", newerUsername, lifetimeUser.Username)
		}
		if lifetimeUser.Discriminator != "0000" {
			t.Fatalf("expected discriminator to be 0000, got %s", lifetimeUser.Discriminator)
		}
	})
}
