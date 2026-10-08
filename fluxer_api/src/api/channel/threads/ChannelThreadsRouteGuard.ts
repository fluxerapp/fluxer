// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID} from '@app/api/BrandedTypes';
import {
	channelThreadsEnabled,
	guildActive,
	userViewerActive,
	viewerFromCtx,
} from '@app/api/experiment/ChannelThreadsGate';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {AppNotFoundHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';
import type {Context} from 'hono';
import {createMiddleware} from 'hono/factory';

const SNOWFLAKE_PARAM = /^\d{1,20}$/;

function callerActive(ctx: Context<HonoEnv>, botOnly: boolean): boolean {
	if (!channelThreadsEnabled()) return false;
	const user = ctx.get('user');
	if (!user || (botOnly && !user.isBot)) return false;
	return userViewerActive(viewerFromCtx(ctx));
}

function pathGuildActive(ctx: Context<HonoEnv>): boolean {
	const guildId = ctx.req.param('guild_id');
	if (guildId !== undefined) {
		return SNOWFLAKE_PARAM.test(guildId) && guildActive(guildId);
	}
	const channelId = ctx.req.param('channel_id');
	if (channelId === undefined) return true;
	if (!SNOWFLAKE_PARAM.test(channelId)) return false;
	const channel = ctx.get('requestCache').channels.get(createChannelID(BigInt(channelId)));
	return channel?.guildId != null && guildActive(channel.guildId);
}

export function ChannelThreadsRouteGuard(options: {botOnly?: boolean} = {}) {
	const botOnly = options.botOnly ?? false;
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		if (!callerActive(ctx, botOnly) || !pathGuildActive(ctx)) {
			return AppNotFoundHandler(ctx);
		}
		return next();
	});
}
