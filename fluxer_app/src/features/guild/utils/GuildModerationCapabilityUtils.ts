// SPDX-License-Identifier: AGPL-3.0-or-later

export interface GuildModerationCapabilityOptions {
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

export function resolveGuildModerationCapabilities(
	options: GuildModerationCapabilityOptions,
): GuildModerationCapabilities {
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
