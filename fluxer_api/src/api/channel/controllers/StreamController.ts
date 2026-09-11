// SPDX-License-Identifier: AGPL-3.0-or-later

import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import {
	StreamPreviewResponse,
	StreamPreviewUploadBodySchema,
	StreamPreviewUploadUrlBodySchema,
	StreamPreviewUploadUrlResponseSchema,
	StreamUpdateBodySchema,
} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import {StreamKeyParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';
import {createChannelID} from '../../BrandedTypes';
import {Config} from '../../Config';
import {DefaultUserOnly, LoginRequired} from '../../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

export function StreamController(app: HonoApp) {
	app.patch(
		'/streams/:stream_key/stream',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_UPDATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', StreamUpdateBodySchema),
		Validator('param', StreamKeyParam),
		OpenAPI({
			operationId: 'update_stream_region',
			summary: 'Update stream region',
			description:
				'Changes the media server region for an active stream. Used to optimise bandwidth and latency for streaming.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {region} = ctx.req.valid('json');
			const streamKey = ctx.req.valid('param').stream_key;
			await ctx.get('streamService').updateStreamRegion({streamKey, region, userId: user.id});
			return ctx.body(null, 204);
		},
	);
	app.get(
		'/streams/:stream_key/preview',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_PREVIEW_GET),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', StreamKeyParam),
		OpenAPI({
			operationId: 'get_stream_preview',
			summary: 'Get stream preview image',
			description:
				'Retrieves the current preview thumbnail for a stream. Returns the image with no-store cache headers to ensure freshness.',
			responseSchema: StreamPreviewResponse,
			responseContentType: 'image/*',
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const streamKey = ctx.req.valid('param').stream_key;
			const preview = await ctx.get('streamService').getPreview({streamKey, userId: user.id});
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
		'/streams/:stream_key/preview/upload-url',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_PREVIEW_UPLOAD_URL),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', StreamPreviewUploadUrlBodySchema),
		Validator('param', StreamKeyParam),
		OpenAPI({
			operationId: 'create_stream_preview_upload_url',
			summary: 'Create stream preview upload URL',
			description: 'Creates a reusable PUT upload URL for updating the current thumbnail image for the stream.',
			responseSchema: StreamPreviewUploadUrlResponseSchema,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {channel_id, content_type} = ctx.req.valid('json');
			const streamKey = ctx.req.valid('param').stream_key;
			const clientIp = requireClientIp(ctx.req.raw, {
				trustClientIpHeader: Config.proxy.trust_client_ip_header,
				clientIpHeaderName: Config.proxy.client_ip_header,
			});
			const response = await ctx.get('streamService').createPreviewUploadUrl({
				streamKey,
				channelId: createChannelID(channel_id),
				userId: user.id,
				contentType: content_type,
				clientIp,
			});
			return ctx.json(response, 200);
		},
	);
	app.post(
		'/streams/:stream_key/preview',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_PREVIEW_POST),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', StreamPreviewUploadBodySchema),
		Validator('param', StreamKeyParam),
		OpenAPI({
			operationId: 'upload_stream_preview',
			summary: 'Upload stream preview image',
			description:
				'Uploads a custom thumbnail image for the stream. The image is scanned for content policy violations and stored securely.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const {thumbnail, channel_id, content_type} = ctx.req.valid('json');
			const streamKey = ctx.req.valid('param').stream_key;
			await ctx.get('streamService').uploadPreview({
				streamKey,
				channelId: createChannelID(channel_id),
				userId: user.id,
				thumbnail,
				contentType: content_type,
			});
			return ctx.body(null, 204);
		},
	);
	app.delete(
		'/streams/:stream_key/preview',
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_STREAM_PREVIEW_DELETE),
		LoginRequired,
		DefaultUserOnly,
		Validator('param', StreamKeyParam),
		OpenAPI({
			operationId: 'delete_stream_preview',
			summary: 'Delete stream preview image',
			description:
				'Removes the thumbnail preview for a stream. Used when the owner opts into "hide preview" so viewers no longer see a stale thumbnail.',
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Channels',
		}),
		async (ctx) => {
			const user = ctx.get('user');
			const streamKey = ctx.req.valid('param').stream_key;
			await ctx.get('streamService').deletePreview({streamKey, userId: user.id});
			return ctx.body(null, 204);
		},
	);
}
