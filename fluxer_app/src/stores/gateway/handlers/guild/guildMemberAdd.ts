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
import type {User} from '~/records/UserRecord';
import GuildMemberStore from '~/stores/GuildMemberStore';
import MemberSearchStore from '~/stores/MemberSearchStore';
import PresenceStore from '~/stores/PresenceStore';
import UserStore from '~/stores/UserStore';
import type {GatewayHandlerContext} from '../index';

interface GuildMemberAddPayload extends GuildMember {
	guild_id: string;
}

export function handleGuildMemberAdd(data: GuildMemberAddPayload, _context: GatewayHandlerContext): void {
	UserStore.handleUserUpdate(data.user as User);
	GuildMemberStore.handleMemberAdd(data.guild_id, data);
	PresenceStore.handleGuildMemberAdd(data.guild_id, data.user.id);
	MemberSearchStore.handleMemberAdd(data.guild_id, data.user.id);
}
