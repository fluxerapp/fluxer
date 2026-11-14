// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Guild, GuildSelfAssignableRole, GuildSelfAssignableRoleSection} from '@app/features/guild/models/Guild';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Permission from '@app/features/permissions/state/Permission';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {canUseSelfAssignableRoleSection} from '@fluxer/constants/src/SelfAssignableRoleRules';

export interface AvailableSelfAssignableRole {
	readonly role: GuildRole;
	readonly description: string;
}

export interface AvailableSelfAssignableRoleSection {
	readonly section: GuildSelfAssignableRoleSection;
	readonly roles: ReadonlyArray<AvailableSelfAssignableRole>;
}

export function resolveSelfAssignableRoles(
	guild: Guild,
	roles: ReadonlyArray<GuildSelfAssignableRole>,
): Array<AvailableSelfAssignableRole> {
	return roles.flatMap(({id, description}) => {
		const role = guild.roles[id];
		return role != null && !role.isEveryone ? [{role, description}] : [];
	});
}

export function getAvailableSelfAssignableRoleSections(guild: Guild): Array<AvailableSelfAssignableRoleSection> {
	return guild.selfAssignableRoleSections.map((section) => ({
		section,
		roles: resolveSelfAssignableRoles(guild, section.roles),
	}));
}

export function getSelfAssignableRoleIds(guild: Guild): ReadonlySet<string> {
	const roleIds = new Set<string>();
	for (const section of guild.selfAssignableRoleSections) {
		for (const role of section.roles) {
			roleIds.add(role.id);
		}
	}
	return roleIds;
}

export function isRoleSelfAssignable(guild: Guild, roleId: string): boolean {
	return guild.selfAssignableRolesEnabled && getSelfAssignableRoleIds(guild).has(roleId);
}

export function getVisibleSelfAssignableRoleSections(
	guild: Guild,
	memberRoleIds: ReadonlySet<string>,
): Array<AvailableSelfAssignableRoleSection> {
	return getAvailableSelfAssignableRoleSections(guild).filter(
		(entry) => entry.roles.length > 0 && canUseSelfAssignableRoleSection(entry.section.requiredRoleIds, memberRoleIds),
	);
}

function currentMemberRoleIds(guildId: string): ReadonlySet<string> {
	const userId = Users.currentUserId;
	return (userId ? GuildMembers.getMember(guildId, userId)?.roles : null) ?? new Set<string>();
}

export function hasAvailableSelfAssignableRoles(guild: Guild): boolean {
	return getVisibleSelfAssignableRoleSections(guild, currentMemberRoleIds(guild.id)).length > 0;
}

export function canEditSelfAssignableRoles(guildId: string): boolean {
	return Permission.can(Permissions.MANAGE_GUILD | Permissions.MANAGE_ROLES, {guildId});
}

export function shouldShowSelfAssignableRolesEntry(guild: Guild): boolean {
	if (!guild.selfAssignableRolesEnabled || guild.visiting) return false;
	return canEditSelfAssignableRoles(guild.id) || hasAvailableSelfAssignableRoles(guild);
}
