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

func TestAdminUsersSetAclsRequiresAclSetUser(t *testing.T) {
	client := newTestClient(t)
	admin := createTestAccount(t, client)
	setUserACLs(t, client, admin.UserID, []string{"admin:authenticate", "user:update:flags"})

	target := createTestAccount(t, client)
	payload := map[string]any{
		"user_id": target.UserID,
		"acls":    []string{"user:update:flags"},
	}

	unauthorizedResp, err := client.postJSONWithAuth("/admin/users/set-acls", payload, admin.Token)
	if err != nil {
		t.Fatalf("failed to call set-acls without acl:set:user: %v", err)
	}
	if unauthorizedResp.StatusCode != http.StatusForbidden {
		t.Fatalf(
			"expected 403 when missing acl:set:user, got %d: %s",
			unauthorizedResp.StatusCode,
			readResponseBody(unauthorizedResp),
		)
	}
	unauthorizedResp.Body.Close()

	setUserACLs(t, client, admin.UserID, []string{"admin:authenticate", "acl:set:user"})

	authorizedResp, err := client.postJSONWithAuth("/admin/users/set-acls", payload, admin.Token)
	if err != nil {
		t.Fatalf("failed to call set-acls with acl:set:user: %v", err)
	}
	if authorizedResp.StatusCode != http.StatusOK {
		t.Fatalf(
			"expected 200 when acl:set:user is present, got %d: %s",
			authorizedResp.StatusCode,
			readResponseBody(authorizedResp),
		)
	}
	authorizedResp.Body.Close()
}
