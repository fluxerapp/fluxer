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

func TestAdminEndpointsRequireAuthAndAcls(t *testing.T) {
	client := newTestClient(t)
	admin := createTestAccount(t, client)

	setUserACLs(t, client, admin.UserID, []string{"admin:authenticate"})

	redirectURI := "https://example.com/callback"
	appID, _, _, _ := createOAuth2Application(
		t,
		client,
		admin,
		fmt.Sprintf("Admin AuthZ %d", time.Now().UnixNano()),
		[]string{redirectURI},
		nil,
	)
	code, _ := authorizeOAuth2(t, client, admin.Token, appID, redirectURI, []string{"identify"}, "", "", "")
	token := exchangeOAuth2AuthorizationCode(t, client, appID, "", code, redirectURI, "").AccessToken

	endpoints := []struct {
		method string
		path   string
	}{
		{http.MethodPost, "/admin/reports/list"},
		{http.MethodGet, "/admin/reports/1"},
		{http.MethodPost, "/admin/reports/resolve"},
		{http.MethodPost, "/admin/reports/search"},
		{http.MethodPost, "/admin/bulk/update-user-flags"},
		{http.MethodPost, "/admin/bulk/update-guild-features"},
		{http.MethodPost, "/admin/bulk/add-guild-members"},
		{http.MethodPost, "/admin/bulk/schedule-user-deletion"},
		{http.MethodPost, "/admin/guilds/search"},
		{http.MethodPost, "/admin/users/search"},
		{http.MethodPost, "/admin/search/refresh-index"},
		{http.MethodPost, "/admin/search/refresh-status"},
		{http.MethodPost, "/admin/voice/regions/list"},
		{http.MethodPost, "/admin/voice/regions/get"},
		{http.MethodPost, "/admin/voice/regions/create"},
		{http.MethodPost, "/admin/voice/regions/update"},
		{http.MethodPost, "/admin/voice/regions/delete"},
		{http.MethodPost, "/admin/voice/servers/list"},
		{http.MethodPost, "/admin/voice/servers/get"},
		{http.MethodPost, "/admin/voice/servers/create"},
		{http.MethodPost, "/admin/voice/servers/update"},
		{http.MethodPost, "/admin/voice/servers/delete"},
		{http.MethodPost, "/admin/pending-verifications/list"},
		{http.MethodPost, "/admin/pending-verifications/approve"},
		{http.MethodPost, "/admin/pending-verifications/reject"},
		{http.MethodPost, "/admin/messages/lookup"},
		{http.MethodPost, "/admin/messages/lookup-by-attachment"},
		{http.MethodPost, "/admin/messages/delete"},
		{http.MethodPost, "/admin/gateway/memory-stats"},
		{http.MethodPost, "/admin/gateway/reload-all"},
		{http.MethodGet, "/admin/gateway/stats"},
		{http.MethodPost, "/admin/audit-logs"},
		{http.MethodPost, "/admin/audit-logs/search"},
		{http.MethodPost, "/admin/guilds/lookup"},
		{http.MethodPost, "/admin/guilds/list-members"},
		{http.MethodPost, "/admin/guilds/clear-fields"},
		{http.MethodPost, "/admin/guilds/update-features"},
		{http.MethodPost, "/admin/guilds/update-name"},
		{http.MethodPost, "/admin/guilds/update-settings"},
		{http.MethodPost, "/admin/guilds/transfer-ownership"},
		{http.MethodPost, "/admin/guilds/update-vanity"},
		{http.MethodPost, "/admin/guilds/force-add-user"},
		{http.MethodPost, "/admin/guilds/reload"},
		{http.MethodPost, "/admin/guilds/shutdown"},
		{http.MethodPost, "/admin/users/lookup"},
		{http.MethodPost, "/admin/users/list-guilds"},
		{http.MethodPost, "/admin/users/disable-mfa"},
		{http.MethodPost, "/admin/users/clear-fields"},
		{http.MethodPost, "/admin/users/set-bot-status"},
		{http.MethodPost, "/admin/users/set-system-status"},
		{http.MethodPost, "/admin/users/verify-email"},
		{http.MethodPost, "/admin/users/send-password-reset"},
		{http.MethodPost, "/admin/users/change-username"},
		{http.MethodPost, "/admin/users/change-email"},
		{http.MethodPost, "/admin/users/terminate-sessions"},
		{http.MethodPost, "/admin/users/temp-ban"},
		{http.MethodPost, "/admin/users/unban"},
		{http.MethodPost, "/admin/users/schedule-deletion"},
		{http.MethodPost, "/admin/users/cancel-deletion"},
		{http.MethodPost, "/admin/users/set-acls"},
		{http.MethodPost, "/admin/users/update-flags"},
		{http.MethodPost, "/admin/users/unlink-phone"},
		{http.MethodPost, "/admin/users/change-dob"},
		{http.MethodPost, "/admin/users/update-suspicious-activity-flags"},
		{http.MethodPost, "/admin/users/disable-suspicious"},
		{http.MethodPost, "/admin/users/list-sessions"},
		{http.MethodPost, "/admin/bans/ip/add"},
		{http.MethodPost, "/admin/bans/ip/remove"},
		{http.MethodPost, "/admin/bans/ip/check"},
		{http.MethodPost, "/admin/bans/email/add"},
		{http.MethodPost, "/admin/bans/email/remove"},
		{http.MethodPost, "/admin/bans/email/check"},
		{http.MethodPost, "/admin/bans/phone/add"},
		{http.MethodPost, "/admin/bans/phone/remove"},
		{http.MethodPost, "/admin/bans/phone/check"},
	}

	for _, ep := range endpoints {
		ep := ep

		t.Run(ep.method+" "+ep.path+" unauthorized", func(t *testing.T) {
			resp := doAdminRequest(t, client, ep.method, ep.path, "")
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusUnauthorized {
				t.Fatalf("expected 401 for %s %s without auth, got %d: %s", ep.method, ep.path, resp.StatusCode, readResponseBody(resp))
			}
		})

		t.Run(ep.method+" "+ep.path+" missing-acl", func(t *testing.T) {
			resp := doAdminRequest(t, client, ep.method, ep.path, token)
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusForbidden {
				t.Fatalf("expected 403 for %s %s with admin:authenticate only, got %d: %s", ep.method, ep.path, resp.StatusCode, readResponseBody(resp))
			}
		})
	}
}
