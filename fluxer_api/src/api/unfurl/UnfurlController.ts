// SPDX-License-Identifier: AGPL-3.0-or-later

import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getUnfurlerService} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {UnfurlRequest, UnfurlResponse} from '@fluxer/schema/src/domains/unfurl/UnfurlSchemas';

export function UnfurlController(app: HonoApp) {
	app.post(
		'/unfurl',
		RateLimitMiddleware(RateLimitConfigs.UNFURL_DEBUG),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'debug_unfurl',
			summary: 'Debug URL unfurl',
			responseSchema: UnfurlResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Debug'],
			description: 'Resolves a single URL through the unfurler without reading from the unfurl cache.',
		}),
		Validator('json', UnfurlRequest),
		async (ctx) => {
			const {url} = ctx.req.valid('json');
			const result = await getUnfurlerService().unfurlWithCachePolicy(url, 'block', {bypassCache: true});
			return ctx.json(result.embeds);
		},
	);
}
