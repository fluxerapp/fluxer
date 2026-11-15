// SPDX-License-Identifier: AGPL-3.0-or-later

import Guilds from '@app/features/guild/state/Guilds';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import {useRoleHierarchy} from '@app/features/permissions/hooks/useRoleHierarchy';
import Permission from '@app/features/permissions/state/Permission';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import type {User} from '@app/features/user/models/User';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';

interface GuildModerationCapabilityOptions {
	isCurrentUser: boolean;
	canManageTarget: boolean;
	canKickMembers: boolean;
	canBanMembers: boolean;
	canModerateMembers: boolean;
	targetHasAdministratorPermission: boolean;
}

export interface GuildModerationCapabilities {
	canKick: boolean;
	canBan: boolean;
	canTimeout: boolean;
}

export type MemberModerationActionKey = 'timeout' | 'remove_timeout' | 'kick' | 'ban';

function resolveGuildModerationCapabilities(options: GuildModerationCapabilityOptions): GuildModerationCapabilities {
	const canModerateTarget = !options.isCurrentUser && options.canManageTarget;
	return {
		canKick: canModerateTarget && options.canKickMembers,
		canBan: canModerateTarget && options.canBanMembers,
		canTimeout: canModerateTarget && options.canModerateMembers && !options.targetHasAdministratorPermission,
	};
}

function resolveMemberModerationActionKeys(options: {
	canTimeout: boolean;
	isTimedOut: boolean;
	canKick: boolean;
	canBan: boolean;
}): Array<MemberModerationActionKey> {
	const actionKeys: Array<MemberModerationActionKey> = [];
	if (options.canTimeout) {
		actionKeys.push(options.isTimedOut ? 'remove_timeout' : 'timeout');
	}
	if (options.canKick) {
		actionKeys.push('kick');
	}
	if (options.canBan) {
		actionKeys.push('ban');
	}
	return actionKeys;
}

export function resolveGuildScopedModerationActionKeys(options: {
	hasMember: boolean;
	canTimeout: boolean;
	isTimedOut: boolean;
	canKick: boolean;
	canBan: boolean;
}): Array<MemberModerationActionKey> {
	return resolveMemberModerationActionKeys({
		canTimeout: options.hasMember && options.canTimeout,
		isTimedOut: options.isTimedOut,
		canKick: options.hasMember && options.canKick,
		canBan: options.canBan,
	});
}

export function useGuildMemberModerationCapabilities(
	guildId: string | null | undefined,
	user: User,
	isCurrentUser: boolean,
	member: GuildMember | null | undefined = null,
) {
	const guild = guildId ? Guilds.getGuild(guildId) : null;
	const {canManageTarget} = useRoleHierarchy(guild);
	const guildSnapshot = guild?.toJSON();
	const canKickMembers = !!guildId && Permission.can(Permissions.KICK_MEMBERS, {guildId});
	const canBanMembers = !!guildId && Permission.can(Permissions.BAN_MEMBERS, {guildId});
	const canModerateMembers = !!guildId && Permission.can(Permissions.MODERATE_MEMBERS, {guildId});
	const targetHasAdministratorPermission =
		guildSnapshot !== undefined && PermissionUtils.can(Permissions.ADMINISTRATOR, user.id, guildSnapshot);
	const canManageNicknames =
		!!guildId &&
		!!member &&
		((isCurrentUser && Permission.can(Permissions.CHANGE_NICKNAME, {guildId}) && !member.isTimedOut()) ||
			(Permission.can(Permissions.MANAGE_NICKNAMES, {guildId}) && canManageTarget(user.id)));
	return {
		...resolveGuildModerationCapabilities({
			isCurrentUser,
			canManageTarget: canManageTarget(user.id),
			canKickMembers,
			canBanMembers,
			canModerateMembers,
			targetHasAdministratorPermission,
		}),
		canManageNicknames,
		canManageTarget,
		guild,
	};
}
