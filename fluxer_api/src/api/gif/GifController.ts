// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GifFeaturedResponse,
	GifLocaleQuery,
	GifRegisterShareRequest,
	GifResponse,
	GifSearchQuery,
} from '@fluxer/schema/src/domains/gif/GifSchemas';
import type {Context, MiddlewareHandler} from 'hono';
import {createMiddleware} from 'hono/factory';
import {z} from 'zod';
import {DefaultUserOnly, LoginRequired} from '../middleware/AuthMiddleware';
import {RateLimitMiddleware} from '../middleware/RateLimitMiddleware';
import {OpenAPI} from '../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../RateLimitConfig';
import type {HonoApp, HonoEnv} from '../types/HonoEnv';
import {Validator} from '../Validator';
import {GifProviderHeaderMiddleware} from './GifProviderHeaderMiddleware';
import {resolveGifRequestCountry} from './GifRequestCountry';

const TAGS = ['GIFs'];

interface PrefixConfig {
	prefix: string;
	tags: ReadonlyArray<string>;
	deprecated: boolean;
	operationSuffix: string;
}

const PREFIXES: ReadonlyArray<PrefixConfig> = [
	{prefix: '/gifs', tags: TAGS, deprecated: false, operationSuffix: 'gifs'},
	{prefix: '/tenor', tags: ['GIFs (Deprecated)'], deprecated: true, operationSuffix: 'tenor'},
	{prefix: '/klipy', tags: ['GIFs (Deprecated)'], deprecated: true, operationSuffix: 'klipy'},
];
const DEPRECATION_NOTICE = 'Use /gifs/* instead - these vendor-specific paths are deprecated and will be removed.';

async function getCountry(ctx: Context<HonoEnv>): Promise<string> {
	return resolveGifRequestCountry(ctx.req.raw);
}

function deprecationMiddleware(deprecated: boolean): MiddlewareHandler<HonoEnv> {
	return createMiddleware<HonoEnv>(async (ctx, next) => {
		await next();
		if (!deprecated) return;
		ctx.header('Deprecation', 'true');
		ctx.header('Link', '</gifs>; rel="successor-version"');
		ctx.header('Warning', `299 - "${DEPRECATION_NOTICE}"`);
	});
}

function registerRoutes(app: HonoApp, cfg: PrefixConfig) {
	const {prefix, tags, deprecated, operationSuffix} = cfg;
	app.use(`${prefix}/*`, GifProviderHeaderMiddleware);
	app.use(`${prefix}/*`, deprecationMiddleware(deprecated));
	app.get(
		`${prefix}/search`,
		RateLimitMiddleware(RateLimitConfigs.GIF_SEARCH),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: `search_${operationSuffix}`,
			summary: `Search GIFs${deprecated ? ' (deprecated alias)' : ''}`,
			responseSchema: z.array(GifResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: [...tags],
			description: deprecated
				? `${DEPRECATION_NOTICE} Routes to the active provider; identical behaviour to /gifs/search.`
				: 'Searches the active GIF provider for GIFs matching the given query. The provider name is returned in the X-Fluxer-GIF-Provider response header so clients can adapt without refetching .well-known.',
		}),
		Validator('query', GifSearchQuery),
		async (ctx) => {
			const {q, locale} = ctx.req.valid('query');
			const provider = await ctx.get('gifService').getActive();
			const country = await getCountry(ctx);
			return ctx.json(await provider.search({q, locale, country}));
		},
	);
	app.get(
		`${prefix}/featured`,
		RateLimitMiddleware(RateLimitConfigs.GIF_FEATURED),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: `get_featured_${operationSuffix}`,
			summary: `Get featured GIFs${deprecated ? ' (deprecated alias)' : ''}`,
			responseSchema: GifFeaturedResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: [...tags],
			description: deprecated
				? `${DEPRECATION_NOTICE} Routes to the active provider; identical behaviour to /gifs/featured.`
				: 'Retrieves currently featured GIFs and category tags from the active provider.',
		}),
		Validator('query', GifLocaleQuery),
		async (ctx) => {
			const provider = await ctx.get('gifService').getActive();
			const country = await getCountry(ctx);
			return ctx.json(await provider.getFeatured({locale: ctx.req.valid('query').locale, country}));
		},
	);
	const trendingPath = prefix === '/gifs' ? `${prefix}/trending` : `${prefix}/trending-gifs`;
	app.get(
		trendingPath,
		RateLimitMiddleware(RateLimitConfigs.GIF_TRENDING),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: `get_trending_${operationSuffix}`,
			summary: `Get trending GIFs${deprecated ? ' (deprecated alias)' : ''}`,
			responseSchema: z.array(GifResponse),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: [...tags],
			description: deprecated
				? `${DEPRECATION_NOTICE} Routes to the active provider; identical behaviour to /gifs/trending.`
				: 'Retrieves trending GIFs from the active provider.',
		}),
		Validator('query', GifLocaleQuery),
		async (ctx) => {
			const provider = await ctx.get('gifService').getActive();
			const country = await getCountry(ctx);
			return ctx.json(
				await provider.getTrendingGifs({
					locale: ctx.req.valid('query').locale,
					country,
				}),
			);
		},
	);
	app.post(
		`${prefix}/register-share`,
		RateLimitMiddleware(RateLimitConfigs.GIF_REGISTER_SHARE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: `register_${operationSuffix}_share`,
			summary: `Register a GIF share${deprecated ? ' (deprecated alias)' : ''}`,
			responseSchema: null,
			statusCode: 204,
			security: ['bearerToken', 'sessionToken'],
			tags: [...tags],
			description: deprecated
				? `${DEPRECATION_NOTICE} Routes to the active provider; identical behaviour to /gifs/register-share.`
				: 'Notifies the active GIF provider that the caller is sharing one of its GIFs.',
		}),
		Validator('json', GifRegisterShareRequest),
		async (ctx) => {
			const {id, q, locale} = ctx.req.valid('json');
			const provider = await ctx.get('gifService').getActive();
			const country = await getCountry(ctx);
			await provider.registerShare({id, q: q ?? '', locale, country});
			return ctx.body(null, 204);
		},
	);
	app.get(
		`${prefix}/suggest`,
		RateLimitMiddleware(RateLimitConfigs.GIF_SUGGEST),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: `get_${operationSuffix}_search_suggestions`,
			summary: `Get GIF search suggestions${deprecated ? ' (deprecated alias)' : ''}`,
			responseSchema: z.array(z.string()),
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: [...tags],
			description: deprecated
				? `${DEPRECATION_NOTICE} Routes to the active provider; identical behaviour to /gifs/suggest.`
				: 'Returns search-term suggestions from the active GIF provider for the given partial query.',
		}),
		Validator('query', GifSearchQuery),
		async (ctx) => {
			const {q, locale} = ctx.req.valid('query');
			const provider = await ctx.get('gifService').getActive();
			return ctx.json(await provider.suggest({q, locale}));
		},
	);
}

export function GifController(app: HonoApp) {
	for (const cfg of PREFIXES) {
		registerRoutes(app, cfg);
	}
}
