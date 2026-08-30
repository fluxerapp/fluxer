// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';
import {type ChannelID, createChannelID, createGuildID, type GuildID} from '../BrandedTypes';
import type {Guild} from '../models/Guild';
import type {User} from '../models/User';
import type {HonoEnv} from '../types/HonoEnv';
import {normalizeRequestPath} from '../utils/RequestPathUtils';

function parseResourceId(path: string, resourceName: 'guilds' | 'channels'): string | null {
	const segments = path.split('/').filter(Boolean);
	if (segments.length < 2 || segments[0] !== resourceName) {
		return null;
	}
	const id = segments[1];
	if (!/^\d+$/.test(id)) {
		return null;
	}
	return id;
}

function extractGuildId(path: string): GuildID | null {
	const guildId = parseResourceId(path, 'guilds');
	if (!guildId) {
		return null;
	}
	return createGuildID(BigInt(guildId));
}

function extractChannelId(path: string): ChannelID | null {
	const channelId = parseResourceId(path, 'channels');
	if (!channelId) {
		return null;
	}
	return createChannelID(BigInt(channelId));
}

function isStaffUser(user: User): boolean {
	return (user.flags & UserFlags.STAFF) === UserFlags.STAFF;
}

function isGuildUnavailableForUser(guild: Guild, user: User): boolean {
	if (guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE)) {
		return true;
	}
	if (guild.features.has(GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF)) {
		return !isStaffUser(user);
	}
	return false;
}

async function resolveGuildIdForRequest(ctx: Context<HonoEnv>, path: string): Promise<GuildID | null> {
	const guildId = extractGuildId(path);
	if (guildId !== null) {
		return guildId;
	}
	const channelId = extractChannelId(path);
	if (channelId === null) {
		return null;
	}
	const channel = await ctx.get('channelRepository').findUnique(channelId);
	ctx.get('requestCache')?.channels.set(channelId, channel);
	return channel?.guildId ?? null;
}

export const GuildAvailabilityMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const user = ctx.get('user');
	if (!user) {
		await next();
		return;
	}
	const normalizedPath = normalizeRequestPath(ctx.req.path);
	const guildId = await resolveGuildIdForRequest(ctx, normalizedPath);
	if (guildId === null) {
		await next();
		return;
	}
	try {
		const guild = await ctx.get('guildService').data.getGuildSystem(guildId);
		ctx.get('requestCache')?.guilds.set(guildId, guild);
		if (isGuildUnavailableForUser(guild, user)) {
			throw new MissingAccessError();
		}
	} catch (error) {
		if (error instanceof UnknownGuildError) {
			await next();
			return;
		}
		throw error;
	}
	await next();
});
