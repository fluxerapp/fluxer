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

import "testing"

func TestDMCallEligibility(t *testing.T) {
	client := newTestClient(t)
	user1 := createTestAccount(t, client)
	user2 := createTestAccount(t, client)

	guild := createGuild(t, client, user1.Token, "Test Guild")
	invite := createChannelInvite(t, client, user1.Token, parseSnowflake(t, guild.SystemChannel))
	joinGuild(t, client, user2.Token, invite.Code)

	createFriendship(t, client, user1, user2)

	dm := createDmChannel(t, client, user1.Token, parseSnowflake(t, user2.UserID))

	t.Run("check call eligibility", func(t *testing.T) {
		eligibility := getCallEligibility(t, client, user1.Token, parseSnowflake(t, dm.ID))

		if !eligibility.Ringable {
			t.Errorf("Expected call to be ringable for fresh users with default settings, got ringable=%v", eligibility.Ringable)
		}

		if eligibility.Silent {
			t.Errorf("Expected call not to be silent for fresh users with default settings, got silent=%v", eligibility.Silent)
		}
	})
}
