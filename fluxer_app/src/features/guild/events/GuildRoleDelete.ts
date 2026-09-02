// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import Permission from '@app/features/permissions/state/Permission';

interface GuildRoleDeletePayload {
	guild_id: string;
	role_id: string;
}

export function handleGuildRoleDelete(data: GuildRoleDeletePayload, _context: GatewayHandlerContext): void {
	Guilds.handleGuildRoleDelete({guildId: data.guild_id, roleId: data.role_id});
	GuildMembers.handleGuildRoleDelete(data.guild_id, data.role_id);
	Channels.handleGuildRoleDelete({guildId: data.guild_id, roleId: data.role_id});
	MemberSidebar.handleGuildStorageIdentityChange(data.guild_id);
	Permission.handleGuildRole(data.guild_id);
	GuildReadState.handleGuildUpdate(data.guild_id);
}
