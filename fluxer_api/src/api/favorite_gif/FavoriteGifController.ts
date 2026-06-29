// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResolveGifUrlsBodySchema, ResolveGifUrlsResponse} from '@fluxer/schema/src/domains/gif/FavoriteGifSchemas';
import type {Context} from 'hono';
import {resolveGifRequestCountry} from '../gif/GifRequestCountry';
import {DefaultUserOnly, LoginRequired} from '../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {getUnfurlerService} from '../middleware/ServiceSingletons';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoApp, HonoEnv} from '../types/HonoEnv';
import {Validator} from '../Validator';
import {resolveFavoriteGifEntry} from './FavoriteGifResolver';

async function getCountry(ctx: Context<HonoEnv>): Promise<string> {
	return resolveGifRequestCountry(ctx.req.raw);
}

function getLocale(ctx: Context<HonoEnv>): string {
	return (ctx.get('requestLocale') || 'en-US').replace('-', '_');
}

export function FavoriteGifController(app: HonoApp) {
	app.post(
		'/users/@me/favorite-gifs/resolve',
		RateLimitMiddleware(RateLimitConfigs.FAVORITE_GIF_RESOLVE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ResolveGifUrlsBodySchema),
		OpenAPI({
			operationId: 'resolve_gif_urls',
			summary: 'Resolve GIF URLs to proxy entries',
			responseSchema: ResolveGifUrlsResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Saved Media'],
			description:
				'Resolves a batch of GIF URLs into entries containing signed media proxy URLs, suitable for storing as URL-only favorite GIFs.',
		}),
		async (ctx) => {
			const {urls} = ctx.req.valid('json');
			const mediaService = ctx.get('mediaService');
			const gifService = ctx.get('gifService');
			const unfurlerService = getUnfurlerService();
			const locale = getLocale(ctx);
			const country = await getCountry(ctx);
			const entries = await Promise.all(
				urls.map((url) => resolveFavoriteGifEntry({url, locale, country, gifService, mediaService, unfurlerService})),
			);
			return ctx.json({entries});
		},
	);
}
