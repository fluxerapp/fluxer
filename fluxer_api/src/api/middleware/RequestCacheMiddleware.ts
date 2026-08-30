// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createMiddleware} from 'hono/factory';
import type {ChannelID, GuildID} from '../BrandedTypes';
import type {Channel} from '../models/Channel';
import type {Guild} from '../models/Guild';
import type {HonoEnv} from '../types/HonoEnv';

export interface RequestCache {
	userPartials: Map<bigint, UserPartialResponse>;
	messageMentionChannels: Map<string, Array<{id: string; name: string; type: number}>>;
	channels: Map<ChannelID, Channel | null>;
	guilds: Map<GuildID, Guild>;
	takeChannel(channelId: ChannelID): Channel | null | undefined;
	takeGuild(guildId: GuildID): Guild | undefined;
	clear(): void;
}

class RequestCacheImpl implements RequestCache {
	userPartials = new Map<bigint, UserPartialResponse>();
	messageMentionChannels = new Map<string, Array<{id: string; name: string; type: number}>>();
	channels = new Map<ChannelID, Channel | null>();
	guilds = new Map<GuildID, Guild>();

	takeChannel(channelId: ChannelID): Channel | null | undefined {
		if (!this.channels.has(channelId)) {
			return undefined;
		}
		const channel = this.channels.get(channelId) ?? null;
		this.channels.delete(channelId);
		return channel;
	}

	takeGuild(guildId: GuildID): Guild | undefined {
		const guild = this.guilds.get(guildId);
		if (guild === undefined) {
			return undefined;
		}
		this.guilds.delete(guildId);
		return guild;
	}

	clear(): void {
		this.userPartials.clear();
		this.messageMentionChannels.clear();
		this.channels.clear();
		this.guilds.clear();
	}
}

export const RequestCacheMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	const requestCache: RequestCache = new RequestCacheImpl();
	ctx.set('requestCache', requestCache);
	try {
		await next();
	} finally {
		requestCache.clear();
	}
});

export function createRequestCache(): RequestCache {
	return new RequestCacheImpl();
}
