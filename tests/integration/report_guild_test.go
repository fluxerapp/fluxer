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

func TestReportGuild(t *testing.T) {
	client := newTestClient(t)
	reporter := createTestAccount(t, client)

	owner := createTestAccount(t, client)
	guild := createGuild(t, client, owner.Token, "Bad Guild")

	channel := createGuildChannel(t, client, owner.Token, parseSnowflake(t, guild.ID), "general")
	invite := createChannelInvite(t, client, owner.Token, parseSnowflake(t, channel.ID))
	joinGuild(t, client, reporter.Token, invite.Code)

	t.Run("can report a guild", func(t *testing.T) {
		req := map[string]any{
			"guild_id":        guild.ID,
			"category":        "illegal_activity",
			"additional_info": "This guild contains illegal content",
		}

		resp, err := client.postJSONWithAuth("/reports/guild", req, reporter.Token)
		if err != nil {
			t.Fatalf("failed to report guild: %v", err)
		}
		defer resp.Body.Close()

		assertStatus(t, resp, http.StatusOK)

		var result map[string]any
		decodeJSONResponse(t, resp, &result)

		if _, ok := result["report_id"]; !ok {
			t.Fatal("expected report_id in response")
		}
	})
}
