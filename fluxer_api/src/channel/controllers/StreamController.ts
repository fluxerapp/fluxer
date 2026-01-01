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

import type {HonoApp} from '~/App';
import {createChannelID} from '~/BrandedTypes';
import {APIErrorCodes} from '~/constants/API';
import {Permissions} from '~/constants/Channel';
import {BadRequestError, MissingPermissionsError} from '~/Errors';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, Int64Type, z} from '~/Schema';
import {Validator} from '~/Validator';

const streamKeyParam = z.object({stream_key: createStringType(1, 256)});

const parseStreamKey = (
	streamKey: string,
): {scope: 'guild' | 'dm'; guildId?: string; channelId: string; connectionId: string} | null => {
	const parts = streamKey.split(':');
	if (parts.length !== 3) return null;
	const [scopeRaw, channelId, connectionId] = parts;
	if (!channelId || !connectionId) return null;
	if (scopeRaw === 'dm') {
		return {scope: 'dm', channelId, connectionId};
	}
	if (!/^[0-9]+$/.test(scopeRaw) || !/^[0-9]+$/.test(channelId)) return null;
	return {scope: 'guild', guildId: scopeRaw, channelId, connectionId};
};

export const StreamController = (app: HonoApp) => {
	app.patch(
		'/streams/:stream_key/stream',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', z.object({region: createStringType(1, 64).optional()})),
		Validator('param', streamKeyParam),
		async (ctx) => {
			const {region} = ctx.req.valid('json');
			const streamKey = ctx.req.valid('param').stream_key;
			await ctx.get('cacheService').set(`stream_region:${streamKey}`, {region, updatedAt: Date.now()}, 60 * 60 * 24);
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/streams/:stream_key/preview',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_PREVIEW_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', streamKeyParam),
		async (ctx) => {
			const streamKey = ctx.req.valid('param').stream_key;
			if (!parseStreamKey(streamKey)) {
				throw new BadRequestError({code: APIErrorCodes.INVALID_REQUEST, message: 'Invalid stream key format'});
			}
			const preview = await ctx.get('streamPreviewService').getPreview(streamKey);
			if (!preview) {
				return ctx.body(null, 404);
			}
			const payload: ArrayBuffer = preview.buffer.slice().buffer;
			const headers = {
				'Content-Type': preview.contentType || 'image/jpeg',
				'Cache-Control': 'no-store, private',
				Pragma: 'no-cache',
			};
			return ctx.newResponse(payload, 200, headers);
		},
	);

	app.post(
		'/streams/:stream_key/preview',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_PREVIEW_POST),
		LoginRequired,
		DefaultUserOnly,
		Validator(
			'json',
			z.object({
				channel_id: Int64Type,
				thumbnail: createStringType(1, 2_000_000),
				content_type: createStringType(1, 64).optional(),
			}),
		),
		Validator('param', streamKeyParam),
		async (ctx) => {
			const user = ctx.get('user');
			const {thumbnail, channel_id, content_type} = ctx.req.valid('json');
			const streamKey = ctx.req.valid('param').stream_key;
			const channelId = createChannelID(channel_id);
			const userId = user.id;

			const parsedKey = parseStreamKey(streamKey);
			if (!parsedKey) {
				throw new BadRequestError({code: APIErrorCodes.INVALID_REQUEST, message: 'Invalid stream key format'});
			}

			const channel = await ctx.get('channelService').getChannel({userId, channelId});

			if (channel.guildId) {
				const hasConnect = await ctx.get('gatewayService').checkPermission({
					guildId: channel.guildId,
					channelId,
					userId,
					permission: Permissions.CONNECT,
				});
				if (!hasConnect) throw new MissingPermissionsError();
			}

			if (channel.guildId && parsedKey.scope !== 'guild') {
				throw new BadRequestError({code: APIErrorCodes.INVALID_REQUEST, message: 'Stream key scope mismatch'});
			}
			if (!channel.guildId && parsedKey.scope !== 'dm') {
				throw new BadRequestError({code: APIErrorCodes.INVALID_REQUEST, message: 'Stream key scope mismatch'});
			}
			if (parsedKey.channelId !== channelId.toString()) {
				throw new BadRequestError({code: APIErrorCodes.INVALID_REQUEST, message: 'Stream key channel mismatch'});
			}

			let body: Uint8Array;
			try {
				body = Uint8Array.from(Buffer.from(thumbnail, 'base64'));
			} catch {
				throw new BadRequestError({
					code: APIErrorCodes.INVALID_REQUEST,
					message: 'Invalid thumbnail payload',
				});
			}
			if (body.byteLength === 0) {
				throw new BadRequestError({
					code: APIErrorCodes.INVALID_REQUEST,
					message: 'Empty thumbnail payload',
				});
			}

			await ctx.get('streamPreviewService').uploadPreview({
				streamKey,
				channelId,
				userId,
				body,
				contentType: content_type,
			});
			return ctx.body(null, 204);
		},
	);
};
