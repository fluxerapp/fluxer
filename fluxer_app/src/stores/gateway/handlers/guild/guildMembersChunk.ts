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

import type {GuildMember} from '~/records/GuildMemberRecord';
import {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {User} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import MemberSearchStore from '~/stores/MemberSearchStore';
import PresenceStore, {type Presence} from '~/stores/PresenceStore';
import UserStore from '~/stores/UserStore';
import type {GatewayHandlerContext} from '../index';

interface GuildMembersChunkPayload {
	guild_id: string;
	members: ReadonlyArray<GuildMember>;
	chunk_index: number;
	chunk_count: number;
	not_found?: ReadonlyArray<string>;
	presences?: ReadonlyArray<Presence>;
	nonce?: string;
}

export function handleGuildMembersChunk(data: GuildMembersChunkPayload, _context: GatewayHandlerContext): void {
	const {guild_id: guildId, members, chunk_index: chunkIndex, chunk_count: chunkCount, presences, nonce} = data;

	for (const member of members) {
		UserStore.handleUserUpdate(member.user as User);
	}

	GuildMemberStore.handleMembersChunk({
		guildId,
		members: members as Array<GuildMember>,
		chunkIndex,
		chunkCount,
		nonce,
	});

	const memberRecords = members.map((member) => new GuildMemberRecord(guildId, member));
	MemberSearchStore.handleMembersChunk(guildId, memberRecords);

	if (presences) {
		for (const presence of presences) {
			PresenceStore.handlePresenceUpdate(presence);
		}
	}
}
