// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveAttachmentForRequester} from '@app/api/attachment/AttachmentMediaAccess';
import {ATTACHMENT_MEDIA_TOKEN_TTL_MS, signAttachmentMediaToken} from '@app/api/attachment/AttachmentMediaToken';
import {fetchAttachmentFromMediaProxy, readTransformQuery} from '@app/api/attachment/AttachmentMediaUpstream';
import {
	type ApiAttachmentLocation,
	parseApiAttachmentRoute,
	parseApiAttachmentUrl,
} from '@app/api/attachment/AttachmentPublicUrl';
import {isApiProxiedAttachments} from '@app/api/instance/AttachmentAccessMode';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp, HonoEnv} from '@app/api/types/HonoEnv';
import {stripApiPrefix} from '@app/api/utils/RequestPathUtils';
import {Validator} from '@app/api/Validator';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {
	AttachmentAccessTokenRequest,
	AttachmentAccessTokenResponse,
	AttachmentMediaResponse,
} from '@fluxer/schema/src/domains/message/AttachmentSchemas';
import type {Context, MiddlewareHandler} from 'hono';

const MAX_STREAMS_PER_USER = 16;
const PASSED_THROUGH_STATUSES = new Set([200, 206, 304, 416]);
const DENIED_UPSTREAM_STATUSES = new Set([403, 404]);
const PASSED_THROUGH_HEADERS = [
	'content-type',
	'content-length',
	'content-range',
	'accept-ranges',
	'content-disposition',
	'etag',
	'last-modified',
] as const;
const STREAMED_MEDIA_PREFIXES = ['audio/', 'video/'];

const openStreams = new Map<string, number>();

function enabledOnly(): MiddlewareHandler<HonoEnv> {
	return async (ctx, next) => {
		if (!isApiProxiedAttachments()) return ctx.notFound();
		return next();
	};
}

function emptyResponse(status: number, extra: Record<string, string> = {}): Response {
	return new Response(null, {status, headers: {'Cache-Control': 'no-store', ...extra}});
}

function notFound(): Response {
	return emptyResponse(404);
}

function requestedLocation(ctx: Context<HonoEnv>): ApiAttachmentLocation | null {
	const url = new URL(ctx.req.url);
	const mids = url.searchParams.getAll('mid');
	if (mids.length > 1) return null;
	return parseApiAttachmentRoute(stripApiPrefix(url.pathname), mids[0] ?? null);
}

function scopeMatches(ctx: Context<HonoEnv>, location: ApiAttachmentLocation): boolean {
	if (ctx.get('authTokenType') !== 'attachment_media') return true;
	const token = ctx.get('attachmentMediaToken');
	return (
		token !== undefined && token.c === location.channelId.toString() && token.a === location.attachmentId.toString()
	);
}

function acquireStream(userKey: string): (() => void) | null {
	const current = openStreams.get(userKey) ?? 0;
	if (current >= MAX_STREAMS_PER_USER) return null;
	openStreams.set(userKey, current + 1);
	let released = false;
	return () => {
		if (released) return;
		released = true;
		const remaining = (openStreams.get(userKey) ?? 1) - 1;
		if (remaining <= 0) openStreams.delete(userKey);
		else openStreams.set(userKey, remaining);
	};
}

function trackedBody(body: ReadableStream<Uint8Array>, release: () => void): ReadableStream<Uint8Array> {
	const reader = body.getReader();
	return new ReadableStream<Uint8Array>(
		{
			async pull(controller) {
				try {
					const {done, value} = await reader.read();
					if (done) {
						release();
						controller.close();
						return;
					}
					controller.enqueue(value);
				} catch (error) {
					release();
					controller.error(error);
				}
			},
			async cancel(reason) {
				release();
				await reader.cancel(reason);
			},
		},
		{highWaterMark: 0},
	);
}

function mediaHeaders(upstream: Response): Headers {
	const headers = new Headers();
	for (const name of PASSED_THROUGH_HEADERS) {
		const value = upstream.headers.get(name);
		if (value !== null) headers.set(name, value);
	}
	const contentType = headers.get('content-type')?.toLowerCase() ?? '';
	const streamed = STREAMED_MEDIA_PREFIXES.some((prefix) => contentType.startsWith(prefix));
	headers.set('Cache-Control', streamed ? 'private, max-age=3600, no-transform' : 'private, max-age=3600');
	headers.set('CDN-Cache-Control', 'no-store');
	headers.set('Vary', 'Authorization');
	headers.set('X-Content-Type-Options', 'nosniff');
	headers.set('Referrer-Policy', 'no-referrer');
	headers.set('Content-Security-Policy', "default-src 'none'; sandbox");
	headers.set('Cross-Origin-Resource-Policy', 'cross-origin');
	return headers;
}

export function AttachmentMediaController(app: HonoApp) {
	app.get(
		'/channels/:channel_id/attachments/:attachment_id/:filename{.+}',
		enabledOnly(),
		async (ctx, next) => {
			if (!ctx.get('user') && ctx.req.query('mt') !== undefined) return emptyResponse(401);
			return next();
		},
		LoginRequired,
		RateLimitMiddleware(RateLimitConfigs.CHANNEL_ATTACHMENT_GET),
		OpenAPI({
			operationId: 'get_attachment_file',
			summary: 'Fetch an attachment file',
			responseSchema: AttachmentMediaResponse,
			responseContentType: 'application/octet-stream',
			statusCode: [200, 206],
			security: ['botToken', 'sessionToken'],
			tags: ['Messages'],
			description:
				'Streams an attachment file after checking that the requester can read the message it belongs to. Only active when the instance serves attachments through the API. Takes the same width, height, format, quality, animated and download query parameters as the media proxy, and supports range requests. A link opened where no header can be sent, such as a video player or a download, can add the mt query parameter with a token for that one file from the attachment token endpoint.',
		}),
		async (ctx) => {
			const location = requestedLocation(ctx);
			if (!location || !scopeMatches(ctx, location)) {
				return location ? emptyResponse(401) : notFound();
			}
			const query = readTransformQuery(new URL(ctx.req.url).searchParams);
			if (query === null) return emptyResponse(400);
			const user = ctx.get('user');
			const storageKey = await resolveAttachmentForRequester(ctx, location);
			if (storageKey === null) return notFound();
			const release = acquireStream(user.id.toString());
			if (release === null) return emptyResponse(429, {'Retry-After': '1'});
			let upstream: Response;
			try {
				upstream = await fetchAttachmentFromMediaProxy({
					storageKey,
					query,
					method: ctx.req.method === 'HEAD' ? 'HEAD' : 'GET',
					headers: ctx.req.raw.headers,
					signal: ctx.req.raw.signal,
				});
			} catch (error) {
				release();
				throw error;
			}
			if (!PASSED_THROUGH_STATUSES.has(upstream.status)) {
				release();
				await upstream.body?.cancel();
				return DENIED_UPSTREAM_STATUSES.has(upstream.status) ? notFound() : emptyResponse(502);
			}
			if (!upstream.body || ctx.req.method === 'HEAD') {
				release();
				await upstream.body?.cancel();
				return new Response(null, {status: upstream.status, headers: mediaHeaders(upstream)});
			}
			return new Response(trackedBody(upstream.body, release), {
				status: upstream.status,
				headers: mediaHeaders(upstream),
			});
		},
	);

	app.post(
		'/users/@me/attachment-token',
		enabledOnly(),
		LoginRequired,
		DefaultUserOnly,
		RateLimitMiddleware(RateLimitConfigs.USER_ATTACHMENT_TOKEN),
		Validator('json', AttachmentAccessTokenRequest),
		OpenAPI({
			operationId: 'create_attachment_token',
			summary: 'Create an attachment token',
			responseSchema: AttachmentAccessTokenResponse,
			statusCode: 200,
			security: ['sessionToken'],
			tags: ['Messages'],
			description:
				'Creates a token for the one attachment in the given link, for places that cannot send a header, such as a video player, a download or a link opened in the browser. The token works for five minutes, only for that file and only while the session that asked for it stays signed in and can still read the message. Only active when the instance serves attachments through the API.',
		}),
		async (ctx) => {
			const session = ctx.get('authSession');
			if (ctx.get('authTokenType') !== 'session' || !session) throw new AccessDeniedError();
			const user = ctx.get('user');
			const location = parseApiAttachmentUrl(ctx.req.valid('json').url);
			if (location === null) return notFound();
			if ((await resolveAttachmentForRequester(ctx, location)) === null) return notFound();
			const expiresAt = Date.now() + ATTACHMENT_MEDIA_TOKEN_TTL_MS;
			const token = signAttachmentMediaToken({
				u: user.id.toString(),
				s: Buffer.from(session.sessionIdHash).toString('base64url'),
				f: [...ctx.get('clientFeatures')],
				c: location.channelId.toString(),
				a: location.attachmentId.toString(),
				expiresAt,
			});
			return ctx.json({token, expires_at: new Date(expiresAt).toISOString()});
		},
	);
}
