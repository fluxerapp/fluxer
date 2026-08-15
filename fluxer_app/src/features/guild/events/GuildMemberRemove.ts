// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import Permission from '@app/features/permissions/state/Permission';
import Presence from '@app/features/presence/state/Presence';
import Users from '@app/features/user/state/Users';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

interface GuildMemberRemovePayload {
	guild_id: string;
	user: UserPartial;
}

export function handleGuildMemberRemove(data: GuildMemberRemovePayload, _context: GatewayHandlerContext): void {
	Users.handleUserUpdate(data.user);
	MemberSearch.handleMemberRemove(data.guild_id, data.user.id);
	GuildMembers.handleMemberRemove(data.guild_id, data.user.id);
	Permission.handleGuildMemberUpdate(data.user.id);
	Presence.handleGuildMemberRemove(data.guild_id, data.user.id);
}
