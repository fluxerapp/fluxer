// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ACTIVITY_PUB_CONTENT_TYPE,
	buildActivityPubActor,
	buildActivityPubOutbox,
	buildWebFinger,
	WEBFINGER_CONTENT_TYPE,
} from '@app/api/activity_pub/ActivityPub';
import {Config} from '@app/api/Config';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {getUnfurlerService} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {AppNotFoundHandler} from '@fluxer/errors/src/domains/core/ErrorHandlers';

const DOCUMENT_CACHE_CONTROL = 'public, max-age=300';

export function ActivityPubController(app: HonoApp) {
	app.get('/activitypub/actor', RateLimitMiddleware(RateLimitConfigs.ACTIVITY_PUB_DOCUMENT), async (ctx) => {
		const publicKeyPem = await getUnfurlerService().activityPubPublicKey();
		if (publicKeyPem === null) {
			return AppNotFoundHandler(ctx);
		}
		return ctx.body(JSON.stringify(buildActivityPubActor(Config.endpoints.apiPublic, publicKeyPem)), 200, {
			'Content-Type': ACTIVITY_PUB_CONTENT_TYPE,
			'Cache-Control': DOCUMENT_CACHE_CONTROL,
		});
	});
	app.get('/activitypub/actor/outbox', RateLimitMiddleware(RateLimitConfigs.ACTIVITY_PUB_DOCUMENT), async (ctx) => {
		if ((await getUnfurlerService().activityPubPublicKey()) === null) {
			return AppNotFoundHandler(ctx);
		}
		return ctx.body(JSON.stringify(buildActivityPubOutbox(Config.endpoints.apiPublic)), 200, {
			'Content-Type': ACTIVITY_PUB_CONTENT_TYPE,
		});
	});
	app.post('/activitypub/actor/inbox', RateLimitMiddleware(RateLimitConfigs.ACTIVITY_PUB_DOCUMENT), async (ctx) => {
		if ((await getUnfurlerService().activityPubPublicKey()) === null) {
			return AppNotFoundHandler(ctx);
		}
		return ctx.body(null, 202);
	});
	app.get('/.well-known/webfinger', RateLimitMiddleware(RateLimitConfigs.ACTIVITY_PUB_DOCUMENT), async (ctx) => {
		if ((await getUnfurlerService().activityPubPublicKey()) === null) {
			return AppNotFoundHandler(ctx);
		}
		const document = buildWebFinger(Config.endpoints.apiPublic, ctx.req.query('resource'));
		if (document === null) {
			return AppNotFoundHandler(ctx);
		}
		return ctx.body(JSON.stringify(document), 200, {
			'Content-Type': WEBFINGER_CONTENT_TYPE,
			'Cache-Control': DOCUMENT_CACHE_CONTROL,
		});
	});
}
