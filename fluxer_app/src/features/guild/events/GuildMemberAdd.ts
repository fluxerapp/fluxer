// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildVerification from '@app/features/guild/state/GuildVerification';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch from '@app/features/member/state/MemberSearch';
import Permission from '@app/features/permissions/state/Permission';
import Presence from '@app/features/presence/state/Presence';
import UserProfile from '@app/features/user/state/UserProfile';
import Users from '@app/features/user/state/Users';
import type {GuildMemberData} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {User} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

interface GuildMemberAddPayload extends GuildMemberData {
	guild_id: string;
}

export function handleGuildMemberAdd(data: GuildMemberAddPayload, _context: GatewayHandlerContext): void {
	Users.handleUserUpdate(data.user as User);
	GuildMembers.handleMemberAdd(data.guild_id, data);
	Permission.handleGuildMemberUpdate(data.user.id);
	GuildVerification.handleGuildMemberUpdate(data.guild_id);
	Presence.handleGuildMemberAdd(data.guild_id, data.user.id);
	MemberSearch.handleMemberAdd(data.guild_id, data.user.id);
	UserProfile.handleGuildMemberAdd(data.user.id);
}
