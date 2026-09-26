// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import Guilds from '@app/features/guild/state/Guilds';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import Permission from '@app/features/permissions/state/Permission';
import type {GuildRole} from '@fluxer/schema/src/domains/guild/GuildRoleSchemas';

interface GuildRoleCreatePayload {
	guild_id: string;
	role: GuildRole;
}

export function handleGuildRoleCreate(data: GuildRoleCreatePayload, _context: GatewayHandlerContext): void {
	Guilds.handleGuildRoleCreate({guildId: data.guild_id, role: data.role});
	MemberSidebar.handleGuildStorageIdentityChange(data.guild_id);
	Permission.handleGuildRole(data.guild_id);
	GuildReadState.handleGuildUpdate(data.guild_id);
}
