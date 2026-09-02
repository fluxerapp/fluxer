// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {
	formatPermissionCategoryLabel,
	getPermissionDescriptionDescriptor,
	getPermissionTitleDescriptor,
	type PermissionScope,
} from '@app/features/permissions/utils/PermissionLabelDescriptors';
import Users from '@app/features/user/state/Users';
import {
	ALL_PERMISSIONS,
	ChannelTypes,
	DEFAULT_PERMISSIONS,
	ElevatedPermissions,
	Permissions,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildMFALevel} from '@fluxer/constants/src/GuildConstants';
import type {RoleId, UserId} from '@fluxer/schema/src/branded/WireIds';
import type {Channel} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {Guild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const NONE = 0n;

interface PermissionOverwrite {
	id: string;
	type: 0 | 1;
	allow: bigint;
	deny: bigint;
}

export interface Role {
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
		userId === Authentication.currentUserId &&
		userId !== guild.owner_id
	) {
		const currentUser = Users.getCurrentUser();
		if (currentUser && !currentUser.mfaEnabled) {
			permissions &= ~ElevatedPermissions;
		}
	}
	return permissions;
}

export function computePermissions(
	user:
		| string
		| {
				id: string;
		  }
		| UserId,
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
		const guildRecord = channel.guild_id != null ? Guilds.getGuild(channel.guild_id) : null;
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
		const guildRecord = Guilds.getGuild(guild.id);
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
	const member = GuildMembers.getMember(guild.id, userId);
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
	if (b == null) return true;
	if (a.position !== b.position) return a.position > b.position;
	return BigInt(a.id) < BigInt(b.id);
}

export function getHighestRole(guild: Guild, userId: string): Role | null {
	const member = GuildMembers.getMember(guild.id, userId);
	if (member == null) return null;
	const guildRecord = Guilds.getGuild(guild.id);
	if (!guildRecord) return null;
	const memberRoleIds = member.roles;
	const memberRoles = Object.values(guildRecord.roles)
		.filter((roleRecord) => memberRoleIds.has(roleRecord.id))
		.sort((a, b) => b.position - a.position || (BigInt(a.id) < BigInt(b.id) ? -1 : 1))
		.map((roleRecord) => ({
			id: roleRecord.id,
			permissions: roleRecord.permissions,
			position: roleRecord.position,
		}));
	return memberRoles[0] ?? null;
}

export function canManageTargetUser(
	guild: Guild,
	userId: string,
	userHighestRole: Role | null,
	targetUserId: string,
): boolean {
	if (guild.owner_id === userId) return true;
	if (!GuildMembers.isMembershipKnown(guild.id, targetUserId)) return false;
	return isRoleHigher(guild, userId, userHighestRole, getHighestRole(guild, targetUserId));
}

export function can(
	permission: bigint,
	user:
		| string
		| {
				id: string;
		  }
		| UserId,
	context: Channel | Guild,
	overwrites?: Record<string, PermissionOverwrite> | null,
	roles?: Record<RoleId, Role> | null,
): boolean {
	return (computePermissions(user, context, overwrites, roles) & permission) === permission;
}

interface PermissionEntryOptions {
	scope?: PermissionScope;
	title?: MessageDescriptor;
	description?: MessageDescriptor | null;
}

const makePermissionEntry = (
	i18n: I18n,
	permission: bigint,
	options: PermissionEntryOptions = {},
): PermissionSpec['permissions'][number] => {
	const scope = options.scope ?? 'guild';
	const titleDescriptor = options.title ?? getPermissionTitleDescriptor(permission, scope);
	const descriptionDescriptor =
		options.description === undefined ? getPermissionDescriptionDescriptor(permission, scope) : options.description;
	if (titleDescriptor == null) {
		throw new Error(`Missing permission label descriptor for ${permission.toString()}`);
	}
	return {
		title: i18n._(titleDescriptor),
		...(descriptionDescriptor != null ? {description: i18n._(descriptionDescriptor)} : {}),
		flag: permission,
	};
};

function generateGuildGeneralPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'communityWide'),
		permissions: [
			makePermissionEntry(i18n, Permissions.ADMINISTRATOR),
			makePermissionEntry(i18n, Permissions.VIEW_AUDIT_LOG),
			makePermissionEntry(i18n, Permissions.MANAGE_GUILD),
			makePermissionEntry(i18n, Permissions.MANAGE_ROLES),
			makePermissionEntry(i18n, Permissions.MANAGE_CHANNELS),
			makePermissionEntry(i18n, Permissions.KICK_MEMBERS),
			makePermissionEntry(i18n, Permissions.BAN_MEMBERS),
			makePermissionEntry(i18n, Permissions.CREATE_INSTANT_INVITE),
			makePermissionEntry(i18n, Permissions.CHANGE_NICKNAME),
			makePermissionEntry(i18n, Permissions.MANAGE_NICKNAMES),
			makePermissionEntry(i18n, Permissions.CREATE_EXPRESSIONS),
			makePermissionEntry(i18n, Permissions.MANAGE_EXPRESSIONS),
			makePermissionEntry(i18n, Permissions.MANAGE_WEBHOOKS),
		],
	};
}

function generateGuildTextPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'messagesMedia'),
		permissions: [
			makePermissionEntry(i18n, Permissions.SEND_MESSAGES),
			makePermissionEntry(i18n, Permissions.SEND_TTS_MESSAGES),
			makePermissionEntry(i18n, Permissions.MANAGE_MESSAGES),
			makePermissionEntry(i18n, Permissions.PIN_MESSAGES),
			makePermissionEntry(i18n, Permissions.EMBED_LINKS),
			makePermissionEntry(i18n, Permissions.ATTACH_FILES),
			makePermissionEntry(i18n, Permissions.READ_MESSAGE_HISTORY),
			makePermissionEntry(i18n, Permissions.MENTION_EVERYONE),
			makePermissionEntry(i18n, Permissions.USE_EXTERNAL_EMOJIS),
			makePermissionEntry(i18n, Permissions.USE_EXTERNAL_STICKERS),
			makePermissionEntry(i18n, Permissions.ADD_REACTIONS),
			makePermissionEntry(i18n, Permissions.BYPASS_SLOWMODE),
		],
	};
}

function generateGuildModerationPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'moderation'),
		permissions: [makePermissionEntry(i18n, Permissions.MODERATE_MEMBERS)],
	};
}

function generateGuildAccessPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'channelAccess'),
		permissions: [
			makePermissionEntry(i18n, Permissions.VIEW_CHANNEL),
			makePermissionEntry(i18n, Permissions.VIEW_CHANNEL_MEMBERS),
		],
	};
}

function generateGuildVoicePermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'audioVideo'),
		permissions: [
			makePermissionEntry(i18n, Permissions.CONNECT),
			makePermissionEntry(i18n, Permissions.SPEAK),
			makePermissionEntry(i18n, Permissions.STREAM),
			makePermissionEntry(i18n, Permissions.USE_VAD),
			makePermissionEntry(i18n, Permissions.PRIORITY_SPEAKER),
			makePermissionEntry(i18n, Permissions.MUTE_MEMBERS),
			makePermissionEntry(i18n, Permissions.DEAFEN_MEMBERS),
			makePermissionEntry(i18n, Permissions.MOVE_MEMBERS),
			makePermissionEntry(i18n, Permissions.UPDATE_RTC_REGION),
		],
	};
}

export function generateChannelGeneralPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'channelManagement'),
		permissions: [
			makePermissionEntry(i18n, Permissions.CREATE_INSTANT_INVITE, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.MANAGE_CHANNELS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.MANAGE_ROLES, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.MANAGE_WEBHOOKS, {scope: 'channel'}),
		],
	};
}

export function generateChannelAccessPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'channelAccess'),
		permissions: [
			makePermissionEntry(i18n, Permissions.VIEW_CHANNEL, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.VIEW_CHANNEL_MEMBERS, {scope: 'channel'}),
		],
	};
}

export function generateChannelTextPermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'messagesMedia'),
		permissions: [
			makePermissionEntry(i18n, Permissions.SEND_MESSAGES, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.MANAGE_MESSAGES, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.PIN_MESSAGES, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.EMBED_LINKS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.ATTACH_FILES, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.READ_MESSAGE_HISTORY, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.MENTION_EVERYONE, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.USE_EXTERNAL_EMOJIS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.USE_EXTERNAL_STICKERS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.ADD_REACTIONS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.BYPASS_SLOWMODE, {scope: 'channel'}),
		],
	};
}

export function generateChannelVoicePermissionSpec(i18n: I18n): PermissionSpec {
	return {
		title: formatPermissionCategoryLabel(i18n, 'audioVideo'),
		permissions: [
			makePermissionEntry(i18n, Permissions.CONNECT, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.SPEAK, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.STREAM, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.USE_VAD, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.PRIORITY_SPEAKER, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.MUTE_MEMBERS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.DEAFEN_MEMBERS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.MOVE_MEMBERS, {scope: 'channel'}),
			makePermissionEntry(i18n, Permissions.UPDATE_RTC_REGION, {scope: 'channel'}),
		],
	};
}

export function generateChannelPermissionSpecs(i18n: I18n, channelType: number): Array<PermissionSpec> {
	const specs = [generateChannelAccessPermissionSpec(i18n), generateChannelGeneralPermissionSpec(i18n)];
	const isCategoryChannel = channelType === ChannelTypes.GUILD_CATEGORY;
	const isVoiceChannel = channelType === ChannelTypes.GUILD_VOICE;
	specs.push(generateChannelTextPermissionSpec(i18n));
	if (isVoiceChannel || isCategoryChannel) {
		specs.push(generateChannelVoicePermissionSpec(i18n));
	}
	return specs;
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

export function getPermissionLabel(i18n: I18n, permission: bigint): string | null {
	const descriptor = getPermissionTitleDescriptor(permission);
	return descriptor != null ? i18n._(descriptor) : null;
}

const UNKNOWN_PERMISSION_DESCRIPTOR = msg({
	message: 'Unknown permission',
	comment: 'Fallback label for a permission bit the client does not recognize.',
});

export function formatPermissionLabel(i18n: I18n, permission: bigint, preferChannelSingular = false): string {
	const descriptor = getPermissionTitleDescriptor(
		permission,
		preferChannelSingular && permission === Permissions.MANAGE_CHANNELS ? 'channel' : 'guild',
	);
	return i18n._(descriptor ?? UNKNOWN_PERMISSION_DESCRIPTOR);
}

export function formatBotPermissionsQuery(permissions: Array<string>): string {
	const total = permissions.reduce((acc, perm) => {
		const key = perm as keyof typeof Permissions;
		const value = Permissions[key];
		return acc | (value ?? 0n);
	}, 0n);
	return total.toString();
}
