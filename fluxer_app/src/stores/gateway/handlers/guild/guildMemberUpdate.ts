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
import EmojiStore from '~/stores/EmojiStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildReadStateStore from '~/stores/GuildReadStateStore';
import GuildVerificationStore from '~/stores/GuildVerificationStore';
import MemberSearchStore from '~/stores/MemberSearchStore';
import MessageStore from '~/stores/MessageStore';
import PermissionStore from '~/stores/PermissionStore';
import PresenceStore from '~/stores/PresenceStore';
import UserStore from '~/stores/UserStore';
import type {GatewayHandlerContext} from '../index';

interface GuildMemberUpdatePayload extends GuildMember {
	guild_id: string;
}

export function handleGuildMemberUpdate(data: GuildMemberUpdatePayload, _context: GatewayHandlerContext): void {
	UserStore.handleUserUpdate(data.user as User);
	GuildMemberStore.handleMemberAdd(data.guild_id, data);
	PermissionStore.handleGuildMemberUpdate(data.user.id);
	GuildReadStateStore.handleGuildMemberUpdate(data.user.id, data.guild_id);
	PresenceStore.handleGuildMemberUpdate(data.guild_id, data.user.id);
	MessageStore.handleGuildMemberUpdate({
		type: 'GUILD_MEMBER_UPDATE',
		guildId: data.guild_id,
		member: data,
	});
	EmojiStore.handleGuildMemberUpdate({guildId: data.guild_id, member: data});
	GuildVerificationStore.handleGuildMemberUpdate(data.guild_id);
	MemberSearchStore.handleMemberUpdate(data.guild_id, data.user.id);
}
