// SPDX-License-Identifier: AGPL-3.0-or-later

import {getFederationIncomingService} from '@app/api/middleware/ServiceSingletons';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';

const MAX_FEDERATION_EVENT_BYTES = 1_048_576;

export function FederationController(app: HonoApp) {
	app.post('/federation/v1/events', RateLimitMiddleware(RateLimitConfigs.ACTIVITY_PUB_DOCUMENT), async (ctx) => {
		const contentLength = Number(ctx.req.header('content-length') ?? '0');
		if (Number.isFinite(contentLength) && contentLength > MAX_FEDERATION_EVENT_BYTES) {
			return ctx.json({error: 'payload_too_large'}, 413);
		}
		const keyId = ctx.req.header('x-fluxer-key-id') ?? '';
		const origin = ctx.req.header('x-fluxer-origin') ?? '';
		if (keyId.length === 0 || origin.length === 0) {
			return ctx.json({error: 'missing_federation_headers'}, 400);
		}
		let body: unknown;
		try {
			body = await ctx.req.json();
		} catch {
			return ctx.json({error: 'invalid_json'}, 400);
		}
		const result = await getFederationIncomingService().receive(body, {keyId, origin});
		if (!result.ok) {
			const status = result.reason === 'unknown_origin' ? 403 : 422;
			return ctx.json({error: result.reason ?? 'invalid_event'}, status);
		}
		return ctx.json({ok: true, event_id: result.eventId}, 202);
	});
}