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
	"math/big"
	"net/http"
	"testing"
)

// TestRoleCreateIgnoresUnknownPermissionBits verifies that creating a role with unknown
// permission bits succeeds and the unknown bits are stripped from the result.
func TestRoleCreateIgnoresUnknownPermissionBits(t *testing.T) {
	client := newTestClient(t)
	owner := createTestAccount(t, client)

	guild := createGuild(t, client, owner.Token, "Unknown Bits Role Create Guild")
	guildID := parseSnowflake(t, guild.ID)

	viewChannel := new(big.Int).Lsh(big.NewInt(1), 10)
	unknownBit := new(big.Int).Lsh(big.NewInt(1), 60)
	combinedPerms := new(big.Int).Or(viewChannel, unknownBit)

	rolePayload := map[string]any{
		"name":        "Role With Unknown Bits",
		"permissions": combinedPerms.String(),
	}

	resp, err := client.postJSONWithAuth(fmt.Sprintf("/guilds/%d/roles", guildID), rolePayload, owner.Token)
	if err != nil {
		t.Fatalf("failed to create role: %v", err)
	}
	assertStatus(t, resp, http.StatusOK)

	var role struct {
		ID          string `json:"id"`
		Name        string `json:"name"`
		Permissions string `json:"permissions"`
	}
	decodeJSONResponse(t, resp, &role)

	returnedPerms, ok := new(big.Int).SetString(role.Permissions, 10)
	if !ok {
		t.Fatalf("failed to parse returned permissions: %s", role.Permissions)
	}

	hasUnknownBit := new(big.Int).And(returnedPerms, unknownBit)
	if hasUnknownBit.Cmp(big.NewInt(0)) != 0 {
		t.Fatalf("expected unknown bit to be stripped, but permissions still contain it: %s", role.Permissions)
	}

	hasViewChannel := new(big.Int).And(returnedPerms, viewChannel)
	if hasViewChannel.Cmp(viewChannel) != 0 {
		t.Fatalf("expected VIEW_CHANNEL bit to be preserved, got permissions: %s", role.Permissions)
	}
}
