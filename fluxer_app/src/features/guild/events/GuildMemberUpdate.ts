// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import Messages from '@app/features/messaging/state/MessagingMessages';
import Permission from '@app/features/permissions/state/Permission';
import Presence from '@app/features/presence/state/Presence';
import Users from '@app/features/user/state/Users';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {User} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export interface GuildMemberUpdatePayload extends GuildMemberData {
	guild_id: string;
}

export function handleGuildMemberUpdate(data: GuildMemberUpdatePayload, _context: GatewayHandlerContext): void {
	Users.handleUserUpdate(data.user as User);
	GuildMembers.handleMemberAdd(data.guild_id, data);
	Permission.handleGuildMemberUpdate(data.user.id);
	GuildReadState.handleGuildMemberUpdate(data.user.id, data.guild_id);
	Presence.handleGuildMemberUpdate(data.guild_id, data.user.id);
	Messages.handleGuildMemberUpdate({
		type: 'GUILD_MEMBER_UPDATE',
		guildId: data.guild_id,
		member: data,
	});
	GuildVerification.handleGuildMemberUpdate(data.guild_id);
	MemberSearch.handleMemberUpdate(data.guild_id, data.user.id);
}
