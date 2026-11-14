// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelOverwriteTypes, ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ValueOf} from '@fluxer/constants/src/ValueOf';

export const GUILD_VISITOR_PERMISSIONS_MASK = Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY;

export const GuildVisitSources = {
	INVITE: 1,
	VANITY_URL: 2,
	DISCOVERY: 3,
	OPERATOR: 4,
} as const;

export type GuildVisitSource = ValueOf<typeof GuildVisitSources>;

export const GuildVisitEndReasons = {
	LEFT: 'left',
	JOINED: 'joined',
	REPLACED: 'replaced',
	BANNED: 'banned',
	DISABLED: 'disabled',
	AGE_RESTRICTED: 'age_restricted',
	EXPIRED: 'expired',
	INVITE_REVOKED: 'invite_revoked',
	BUSY: 'busy',
} as const;

export type GuildVisitEndReason = ValueOf<typeof GuildVisitEndReasons>;

export const GUILD_VISIT_TTL_SECONDS = 1_209_600;
export const GUILD_VISIT_OPEN_REFRESH_SECONDS = 86_400;

interface GuildVisitorOverwrite {
	readonly id: string;
	readonly type: number;
	readonly allow: string;
	readonly deny: string;
}

export interface GuildVisitorChannelInput {
	readonly id: string;
	readonly type: number;
	readonly nsfw_override?: boolean | null;
	readonly permission_overwrites?: ReadonlyArray<GuildVisitorOverwrite> | null;
}

export interface GuildVisitorPermissionInput {
	guildId: string;
	everyonePermissions: bigint;
	guildNsfw: boolean;
	channel: GuildVisitorChannelInput;
	parent?: {readonly nsfw_override?: boolean | null} | null;
	thread?: {private: boolean} | null;
	adultContentAllowed: boolean;
}

function parseBits(value: string): bigint {
	try {
		return BigInt(value);
	} catch {
		return 0n;
	}
}

function isGuildVisitorChannelNsfw(input: {
	guildNsfw: boolean;
	channel: GuildVisitorChannelInput;
	parent?: {readonly nsfw_override?: boolean | null} | null;
}): boolean {
	const own = input.channel.nsfw_override;
	if (own !== null && own !== undefined) return own;
	if (input.channel.type !== ChannelTypes.GUILD_CATEGORY) {
		const inherited = input.parent?.nsfw_override;
		if (inherited !== null && inherited !== undefined) return inherited;
	}
	return input.guildNsfw;
}

export function computeGuildVisitorChannelPermissions(input: GuildVisitorPermissionInput): bigint {
	if (input.thread?.private) return 0n;
	if (!input.adultContentAllowed && isGuildVisitorChannelNsfw(input)) return 0n;
	let permissions = input.everyonePermissions & ~Permissions.ADMINISTRATOR;
	for (const overwrite of input.channel.permission_overwrites ?? []) {
		if (overwrite.type !== ChannelOverwriteTypes.ROLE || overwrite.id !== input.guildId) continue;
		permissions &= ~parseBits(overwrite.deny);
		permissions |= parseBits(overwrite.allow);
	}
	return permissions & GUILD_VISITOR_PERMISSIONS_MASK;
}
