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

func TestAuthBetaCodeRedemptionFlow(t *testing.T) {
	client := newTestClient(t)

	sponsor := createTestAccount(t, client)
	pending := createTestAccount(t, client, withBetaCode("NOVERIFY"))

	initialList := fetchBetaCodes(t, client, sponsor.Token)
	if initialList.Allowance == 0 {
		t.Fatalf("expected initial beta code allowance to be positive")
	}

	codeForRedeem := createBetaCode(t, client, sponsor.Token)
	codeToDelete := createBetaCode(t, client, sponsor.Token)

	deleteBetaCode(t, client, sponsor.Token, codeToDelete)

	listAfterDelete := fetchBetaCodes(t, client, sponsor.Token)
	foundRedeem := false
	for _, entry := range listAfterDelete.BetaCodes {
		if entry.Code == codeToDelete {
			t.Fatalf("expected deleted beta code %s to be removed", codeToDelete)
		}
		if entry.Code == codeForRedeem {
			foundRedeem = true
		}
	}
	if !foundRedeem {
		t.Fatalf("expected redeem beta code %s in list", codeForRedeem)
	}

	resp, err := client.postJSONWithAuth("/auth/redeem-beta-code", map[string]string{"beta_code": codeForRedeem}, pending.Token)
	if err != nil {
		t.Fatalf("failed to redeem beta code: %v", err)
	}
	assertStatus(t, resp, http.StatusNoContent)
	resp.Body.Close()

	login := loginTestUser(t, client, pending.Email, pending.Password)
	if login.PendingVerification != nil && *login.PendingVerification {
		t.Fatalf("expected pending verification cleared after beta redemption")
	}
}
