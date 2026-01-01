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

import {createMiddleware} from 'hono/factory';
import type {HonoApp, HonoEnv} from '~/App';
import {Config} from '~/Config';
import {Locales} from '~/Constants';
import {MissingAccessError} from '~/Errors';
import {Logger} from '~/Logger';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {createStringType, z} from '~/Schema';
import {Validator} from '~/Validator';

const tenorApiKeyRequiredMiddleware = createMiddleware<HonoEnv>(async (_ctx, next) => {
	if (!Config.tenor.apiKey) {
		Logger.debug('Tenor API key is missing');
		throw new MissingAccessError();
	}
	await next();
});

const LocaleType = z
	.enum(Object.values(Locales))
	.default('en-US')
	.transform((v) => v.replace('-', '_'));

export const TenorController = (app: HonoApp) => {
	app.get(
		'/tenor/search',
		RateLimitMiddleware(RateLimitConfigs.TENOR_SEARCH),
		tenorApiKeyRequiredMiddleware,
		LoginRequired,
		DefaultUserOnly,
		Validator('query', z.object({q: createStringType(), locale: LocaleType})),
		async (ctx) => {
			const {q, locale} = ctx.req.valid('query');
			return ctx.json(await ctx.get('tenorService').search({q, locale, ctx}));
		},
	);

	app.get(
		'/tenor/featured',
		RateLimitMiddleware(RateLimitConfigs.TENOR_FEATURED),
		tenorApiKeyRequiredMiddleware,
		LoginRequired,
		DefaultUserOnly,
		Validator('query', z.object({locale: LocaleType})),
		async (ctx) => {
			return ctx.json(await ctx.get('tenorService').getFeatured({locale: ctx.req.valid('query').locale, ctx}));
		},
	);

	app.get(
		'/tenor/trending-gifs',
		RateLimitMiddleware(RateLimitConfigs.TENOR_TRENDING),
		tenorApiKeyRequiredMiddleware,
		LoginRequired,
		DefaultUserOnly,
		Validator('query', z.object({locale: LocaleType})),
		async (ctx) => {
			return ctx.json(await ctx.get('tenorService').getTrendingGifs({locale: ctx.req.valid('query').locale, ctx}));
		},
	);

	app.post(
		'/tenor/register-share',
		RateLimitMiddleware(RateLimitConfigs.TENOR_REGISTER_SHARE),
		tenorApiKeyRequiredMiddleware,
		LoginRequired,
		DefaultUserOnly,
		Validator('json', z.object({id: createStringType(), q: createStringType(0).nullish(), locale: LocaleType})),
		async (ctx) => {
			const {id, q, locale} = ctx.req.valid('json');
			await ctx.get('tenorService').registerShare({id, q: q ?? '', locale, ctx});
			return ctx.body(null, 204);
		},
	);

	app.get(
		'/tenor/suggest',
		RateLimitMiddleware(RateLimitConfigs.TENOR_SUGGEST),
		tenorApiKeyRequiredMiddleware,
		LoginRequired,
		DefaultUserOnly,
		Validator('query', z.object({q: createStringType(), locale: LocaleType})),
		async (ctx) => {
			const {q, locale} = ctx.req.valid('query');
			return ctx.json(await ctx.get('tenorService').suggest({q, locale, ctx}));
		},
	);
};
