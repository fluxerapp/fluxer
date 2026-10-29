// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildRole} from '@app/features/guild/models/GuildRole';

export const ROLE_DND_TYPE = 'guild-role';
export const GUILD_ROLES_TAB_ID = 'roles';

export interface RoleDragItem {
	type: typeof ROLE_DND_TYPE;
	id: string;
	isEveryone: boolean;
	isLocked: boolean;
}

export interface RoleUpdate {
	id: string;
	name?: string;
	color?: number;
	hoist?: boolean;
	mentionable?: boolean;
	permissions?: bigint;
	icon?: string | null;
	unicodeEmoji?: string | null;
}

export function applyRoleUpdate(role: GuildRole, updates: RoleUpdate | undefined): GuildRole {
	if (!updates) return role;
	return role.withUpdates({
		name: updates.name ?? role.name,
		color: updates.color ?? role.color,
		hoist: updates.hoist ?? role.hoist,
		mentionable: updates.mentionable ?? role.mentionable,
		permissions: updates.permissions?.toString() ?? role.permissions.toString(),
		position: role.position,
		icon: updates.icon !== undefined ? updates.icon : role.icon,
		unicode_emoji: updates.unicodeEmoji !== undefined ? updates.unicodeEmoji : role.unicodeEmoji,
	});
}
