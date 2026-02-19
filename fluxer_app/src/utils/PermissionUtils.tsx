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

import AuthenticationStore from '@app/stores/AuthenticationStore';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import GuildStore from '@app/stores/GuildStore';
import UserStore from '@app/stores/UserStore';
import {
	ALL_PERMISSIONS,
	DEFAULT_PERMISSIONS,
	ElevatedPermissions,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import type {RoleId, UserId} from '@fluxer/schema/src/branded/WireIds';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Guild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const NONE = 0n;

interface PermissionOverwrite {
	id: string;
	type: 0 | 1;
	allow: bigint;
	deny: bigint;
}

interface Role {
	id: string;
	permissions: bigint;
	position: number;
}

export interface PermissionSpec {
	title: string;
	permissions: Array<{
		title: string;
		description?: string;
		flag: bigint;
	}>;
}

function calculateElevatedPermissions(permissions: bigint, guild: Guild, userId: string, checkElevated = true): bigint {
	if (
		checkElevated &&
		(guild.mfa_level ?? 0) === GuildMFALevel.ELEVATED &&
		userId === AuthenticationStore.currentUserId
	) {
		const currentUser = UserStore.getCurrentUser();
		if (currentUser && !currentUser.mfaEnabled) {
			permissions &= ~ElevatedPermissions;
		}
	}
	return permissions;
}

export function computePermissions(
	user: string | {id: string} | UserId,
	context: Channel | Guild,
	overwrites?: Record<string, PermissionOverwrite> | null,
	roles?: Record<RoleId, Role> | null,
	checkElevated = true,
): bigint {
	const userId = typeof user === 'string' ? user : user.id;
	let guild: Guild | null = null;
	let guildRoles: Record<RoleId, Role> | null = null;

	if ('guild_id' in context) {
		const channel = context as Channel;
		const channelOverwrites = channel.permission_overwrites ?? [];
		const convertedOverwrites = Object.fromEntries(
			channelOverwrites.map((ow) => [
				ow.id,
				{id: ow.id, type: ow.type, allow: BigInt(ow.allow), deny: BigInt(ow.deny)} as PermissionOverwrite,
			]),
		) as Record<string, PermissionOverwrite>;
		overwrites = overwrites != null ? {...convertedOverwrites, ...overwrites} : convertedOverwrites;
		const guildRecord = channel.guild_id != null ? GuildStore.getGuild(channel.guild_id) : null;
		if (guildRecord) {
			guild = guildRecord.toJSON();
			guildRoles = Object.fromEntries(
				Object.entries(guildRecord.roles).map(([id, roleRecord]) => [
					id,
					{
						id: roleRecord.id,
						permissions: roleRecord.permissions,
						position: roleRecord.position,
					},
				]),
			);
		}
	} else {
		overwrites = overwrites || {};
		guild = context as Guild;
		const guildRecord = GuildStore.getGuild(guild.id);
		if (guildRecord) {
			guildRoles = Object.fromEntries(
				Object.entries(guildRecord.roles).map(([id, roleRecord]) => [
					id,
					{
						id: roleRecord.id,
						permissions: roleRecord.permissions,
						position: roleRecord.position,
					},
				]),
			);
		}
	}

	if (guild == null) {
		return NONE;
	}

	if (guild.owner_id === userId) {
		return calculateElevatedPermissions(ALL_PERMISSIONS, guild, userId, checkElevated);
	}

	roles = roles != null && guildRoles ? {...guildRoles, ...roles} : (guildRoles ?? roles ?? {});

	const member = GuildMemberStore.getMember(guild.id, userId);

	const roleEveryone = roles?.[guild.id as keyof typeof roles];
	let permissions = roleEveryone != null ? roleEveryone.permissions : DEFAULT_PERMISSIONS;

	if (member != null && roles) {
		for (const roleId of member.roles) {
			const role = roles[roleId as keyof typeof roles];
			if (role !== undefined) {
				permissions |= role.permissions;
			}
		}
	}

	if ((permissions & Permissions.ADMINISTRATOR) === Permissions.ADMINISTRATOR) {
		permissions = ALL_PERMISSIONS;
	} else if (overwrites) {
		const overwriteEveryone = overwrites[guild.id];
		if (overwriteEveryone != null) {
			permissions ^= permissions & overwriteEveryone.deny;
			permissions |= overwriteEveryone.allow;
		}

		if (member != null) {
			let allow = NONE;
			let deny = NONE;

			for (const roleId of member.roles) {
				const overwriteRole = overwrites[roleId as string];
				if (overwriteRole != null) {
					allow |= overwriteRole.allow;
					deny |= overwriteRole.deny;
				}
			}

			permissions ^= permissions & deny;
			permissions |= allow;

			const overwriteMember = overwrites[userId];
			if (overwriteMember != null) {
				permissions ^= permissions & overwriteMember.deny;
				permissions |= overwriteMember.allow;
			}
		}
	}

	return calculateElevatedPermissions(permissions, guild, userId, checkElevated);
}

export function isRoleHigher(guild: Guild, userId: string, a: Role | null, b: Role | null): boolean {
	if (guild.owner_id === userId) return true;
	if (a == null) return false;

	const guildRecord = GuildStore.getGuild(guild.id);
	if (!guildRecord) return false;

	const rolesList = Object.values(guildRecord.roles)
		.sort((r1, r2) => r1.position - r2.position)
		.map((role) => role.id);

	return rolesList.indexOf(a.id) > (b != null ? rolesList.indexOf(b.id) : -1);
}

export function getHighestRole(guild: Guild, userId: string): Role | null {
	const member = GuildMemberStore.getMember(guild.id, userId);
	if (member == null) return null;

	const guildRecord = GuildStore.getGuild(guild.id);
	if (!guildRecord) return null;

	const memberRoles = Object.values(guildRecord.roles)
		.filter((roleRecord) => Array.from(member.roles).includes(roleRecord.id))
		.sort((a, b) => b.position - a.position)
		.map((roleRecord) => ({
			id: roleRecord.id,
			permissions: roleRecord.permissions,
			position: roleRecord.position,
		}));

	return memberRoles[0] ?? null;
}

export function can(
	permission: bigint,
	user: string | {id: string} | UserId,
	context: Channel | Guild,
	overwrites?: Record<string, PermissionOverwrite> | null,
	roles?: Record<RoleId, Role> | null,
): boolean {
	return (computePermissions(user, context, overwrites, roles) & permission) === permission;
}

function generateGuildGeneralPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Community-wide`),
		permissions: [
			{
				title: i18n._(msg`Administrator`),
				description: i18n._(msg`Grants all permissions and bypasses channel restrictions. Highly sensitive.`),
				flag: Permissions.ADMINISTRATOR,
			},
			{
				title: i18n._(msg`View Activity Log`),
				description: i18n._(msg`Read the community's audit log of changes and moderation actions.`),
				flag: Permissions.VIEW_AUDIT_LOG,
			},
			{
				title: i18n._(msg`Manage Community`),
				description: i18n._(msg`Edit global settings like name, description, and icon.`),
				flag: Permissions.MANAGE_GUILD,
			},
			{
				title: i18n._(msg`Manage Roles`),
				description: i18n._(
					msg`Create, edit, or delete roles below your highest role. Also allows editing channel permission overwrites.`,
				),
				flag: Permissions.MANAGE_ROLES,
			},
			{
				title: i18n._(msg`Manage Channels`),
				description: i18n._(msg`Create, edit, or delete channels and categories.`),
				flag: Permissions.MANAGE_CHANNELS,
			},
			{
				title: i18n._(msg`Kick Members`),
				flag: Permissions.KICK_MEMBERS,
			},
			{
				title: i18n._(msg`Ban Members`),
				flag: Permissions.BAN_MEMBERS,
			},
			{
				title: i18n._(msg`Create Invite Links`),
				flag: Permissions.CREATE_INSTANT_INVITE,
			},
			{
				title: i18n._(msg`Change Own Nickname`),
				description: i18n._(msg`Update your own nickname.`),
				flag: Permissions.CHANGE_NICKNAME,
			},
			{
				title: i18n._(msg`Manage Nicknames`),
				description: i18n._(msg`Change other members' nicknames.`),
				flag: Permissions.MANAGE_NICKNAMES,
			},
			{
				title: i18n._(msg`Create Emoji & Stickers`),
				description: i18n._(msg`Upload new emoji and stickers, and manage your own creations.`),
				flag: Permissions.CREATE_EXPRESSIONS,
			},
			{
				title: i18n._(msg`Manage Emoji & Stickers`),
				description: i18n._(msg`Edit or delete emoji and stickers created by other members.`),
				flag: Permissions.MANAGE_EXPRESSIONS,
			},
			{
				title: i18n._(msg`Manage Webhooks`),
				description: i18n._(msg`Create, edit, or delete webhooks.`),
				flag: Permissions.MANAGE_WEBHOOKS,
			},
		],
	};
}

function generateGuildTextPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Messages & Media`),
		permissions: [
			{
				title: i18n._(msg`Send Messages`),
				flag: Permissions.SEND_MESSAGES,
			},
			{
				title: i18n._(msg`Send TTS Messages`),
				description: i18n._(msg`Send text-to-speech messages.`),
				flag: Permissions.SEND_TTS_MESSAGES,
			},
			{
				title: i18n._(msg`Manage Messages`),
				description: i18n._(msg`Delete others' messages. (Pinning is separate below.)`),
				flag: Permissions.MANAGE_MESSAGES,
			},
			{
				title: i18n._(msg`Pin Messages`),
				flag: Permissions.PIN_MESSAGES,
			},
			{
				title: i18n._(msg`Embed Links`),
				flag: Permissions.EMBED_LINKS,
			},
			{
				title: i18n._(msg`Attach Files`),
				flag: Permissions.ATTACH_FILES,
			},
			{
				title: i18n._(msg`Read Message History`),
				flag: Permissions.READ_MESSAGE_HISTORY,
			},
			{
				title: i18n._(msg`Use @everyone/@here and @roles`),
				description: i18n._(msg`Mention everyone or any role (even if the role isn't set to be mentionable).`),
				flag: Permissions.MENTION_EVERYONE,
			},
			{
				title: i18n._(msg`Use External Emoji`),
				description: i18n._(msg`Use emoji from other communities.`),
				flag: Permissions.USE_EXTERNAL_EMOJIS,
			},
			{
				title: i18n._(msg`Use External Stickers`),
				flag: Permissions.USE_EXTERNAL_STICKERS,
			},
			{
				title: i18n._(msg`Add Reactions`),
				description: i18n._(msg`Add new reactions to messages.`),
				flag: Permissions.ADD_REACTIONS,
			},
			{
				title: i18n._(msg`Bypass Slowmode`),
				description: i18n._(msg`Ignore per-channel message rate limits.`),
				flag: Permissions.BYPASS_SLOWMODE,
			},
		],
	};
}

function generateGuildModerationPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Moderation`),
		permissions: [
			{
				title: i18n._(msg`Timeout Members`),
				description: i18n._(msg`Prevent members from sending messages, reacting, and joining voice for a duration.`),
				flag: Permissions.MODERATE_MEMBERS,
			},
		],
	};
}

function generateGuildAccessPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Channel Access`),
		permissions: [{title: i18n._(msg`View Channel`), flag: Permissions.VIEW_CHANNEL}],
	};
}

function generateGuildVoicePermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Audio & Video`),
		permissions: [
			{
				title: i18n._(msg`Connect (Join Voice)`),
				flag: Permissions.CONNECT,
			},
			{
				title: i18n._(msg`Speak`),
				flag: Permissions.SPEAK,
			},
			{
				title: i18n._(msg`Stream Video`),
				flag: Permissions.STREAM,
			},
			{
				title: i18n._(msg`Use Voice Activity`),
				description: i18n._(msg`Otherwise Push-to-talk is required.`),
				flag: Permissions.USE_VAD,
			},
			{
				title: i18n._(msg`Priority Speaker`),
				flag: Permissions.PRIORITY_SPEAKER,
			},
			{
				title: i18n._(msg`Mute Members`),
				flag: Permissions.MUTE_MEMBERS,
			},
			{
				title: i18n._(msg`Deafen Members`),
				flag: Permissions.DEAFEN_MEMBERS,
			},
			{
				title: i18n._(msg`Move Members`),
				description: i18n._(msg`Drag members between channels they can access.`),
				flag: Permissions.MOVE_MEMBERS,
			},
			{
				title: i18n._(msg`Set Voice Region`),
				flag: Permissions.UPDATE_RTC_REGION,
			},
		],
	};
}

export function generateChannelGeneralPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Channel Management`),
		permissions: [
			{
				title: i18n._(msg`Create Invite Links`),
				flag: Permissions.CREATE_INSTANT_INVITE,
			},
			{
				title: i18n._(msg`Manage Channel`),
				description: i18n._(msg`Rename and edit this channel's settings.`),
				flag: Permissions.MANAGE_CHANNELS,
			},
			{
				title: i18n._(msg`Manage Permissions`),
				description: i18n._(msg`Edit overwrites for roles and members in this channel.`),
				flag: Permissions.MANAGE_ROLES,
			},
			{
				title: i18n._(msg`Manage Webhooks`),
				description: i18n._(msg`Create, edit, or delete webhooks for this channel.`),
				flag: Permissions.MANAGE_WEBHOOKS,
			},
		],
	};
}

export function generateChannelAccessPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Channel Access`),
		permissions: [{title: i18n._(msg`View Channel`), flag: Permissions.VIEW_CHANNEL}],
	};
}

export function generateChannelTextPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Messages & Media`),
		permissions: [
			{title: i18n._(msg`Send Messages`), flag: Permissions.SEND_MESSAGES},
			{
				title: i18n._(msg`Manage Messages`),
				description: i18n._(msg`Delete others' messages. (Pinning is separate below.)`),
				flag: Permissions.MANAGE_MESSAGES,
			},
			{title: i18n._(msg`Pin Messages`), flag: Permissions.PIN_MESSAGES},
			{title: i18n._(msg`Embed Links`), flag: Permissions.EMBED_LINKS},
			{title: i18n._(msg`Attach Files`), flag: Permissions.ATTACH_FILES},
			{title: i18n._(msg`Read Message History`), flag: Permissions.READ_MESSAGE_HISTORY},
			{
				title: i18n._(msg`Use @everyone/@here and @roles`),
				description: i18n._(msg`Mention everyone or any role (even if the role isn't set to be mentionable).`),
				flag: Permissions.MENTION_EVERYONE,
			},
			{title: i18n._(msg`Use External Emoji`), flag: Permissions.USE_EXTERNAL_EMOJIS},
			{title: i18n._(msg`Use External Stickers`), flag: Permissions.USE_EXTERNAL_STICKERS},
			{
				title: i18n._(msg`Add Reactions`),
				description: i18n._(msg`Add new reactions to messages.`),
				flag: Permissions.ADD_REACTIONS,
			},
			{
				title: i18n._(msg`Bypass Slowmode`),
				description: i18n._(msg`Ignore per-channel message rate limits.`),
				flag: Permissions.BYPASS_SLOWMODE,
			},
		],
	};
}

export function generateChannelVoicePermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: i18n._(msg`Audio & Video`),
		permissions: [
			{title: i18n._(msg`Connect (Join Voice)`), flag: Permissions.CONNECT},
			{title: i18n._(msg`Speak`), flag: Permissions.SPEAK},
			{title: i18n._(msg`Stream Video`), flag: Permissions.STREAM},
			{
				title: i18n._(msg`Use Voice Activity`),
				description: i18n._(msg`Otherwise Push-to-talk is required.`),
				flag: Permissions.USE_VAD,
			},
			{title: i18n._(msg`Priority Speaker`), flag: Permissions.PRIORITY_SPEAKER},
			{title: i18n._(msg`Mute Members`), flag: Permissions.MUTE_MEMBERS},
			{title: i18n._(msg`Deafen Members`), flag: Permissions.DEAFEN_MEMBERS},
			{
				title: i18n._(msg`Move Members`),
				description: i18n._(msg`Drag members between channels they can access.`),
				flag: Permissions.MOVE_MEMBERS,
			},
			{title: i18n._(msg`Set Voice Region`), flag: Permissions.UPDATE_RTC_REGION},
		],
	};
}

export function generatePermissionSpec(i18n: I18n): Array<PermissionSpec> {
	return [
		generateGuildGeneralPermissionSpec(i18n),
		generateGuildAccessPermissionSpec(i18n),
		generateGuildTextPermissionSpec(i18n),
		generateGuildModerationPermissionSpec(i18n),
		generateGuildVoicePermissionSpec(i18n),
	];
}

export interface BotPermissionOption {
	id: keyof typeof Permissions;
	label: string;
}

export function getAllBotPermissions(i18n: I18n): Array<BotPermissionOption> {
	return generatePermissionSpec(i18n).flatMap((spec) =>
		spec.permissions.map((perm) => ({
			id: Object.keys(Permissions).find(
				(key) => Permissions[key as keyof typeof Permissions] === perm.flag,
			) as keyof typeof Permissions,
			label: perm.title,
		})),
	);
}

const permissionLabelCache = new Map<bigint, string>();

const populatePermissionLabels = (i18n: I18n): void => {
	if (permissionLabelCache.size > 0) return;
	for (const {permissions} of generatePermissionSpec(i18n)) {
		for (const perm of permissions) {
			permissionLabelCache.set(perm.flag, perm.title);
		}
	}
};

export function getPermissionLabel(i18n: I18n, permission: bigint): string | null {
	populatePermissionLabels(i18n);
	return permissionLabelCache.get(permission) ?? null;
}

export function formatPermissionLabel(i18n: I18n, permission: bigint, preferChannelSingular = false): string | null {
	if (permission === Permissions.MANAGE_CHANNELS) {
		return preferChannelSingular ? i18n._(msg`Manage Channel`) : i18n._(msg`Manage Channels`);
	}
	return getPermissionLabel(i18n, permission);
}

export function formatBotPermissionsQuery(permissions: Array<string>): string {
	const total = permissions.reduce((acc, perm) => {
		const key = perm as keyof typeof Permissions;
		const value = Permissions[key];
		return acc | (value ?? 0n);
	}, 0n);
	return total.toString();
}
