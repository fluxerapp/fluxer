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

import {randomBytes} from 'node:crypto';
import type {HonoApp} from '~/App';
import {Config} from '~/Config';
import {FileSizeTooLargeError} from '~/errors/FileSizeTooLargeError';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {z} from '~/Schema';
import {Validator} from '~/Validator';

const ThemeShareBodySchema = z.object({
	css: z.string().min(1),
});

const MAX_CSS_BYTES = 8 * 1024 * 1024;

export const ThemeController = (app: HonoApp) => {
	app.post(
		'/users/@me/themes',
		RateLimitMiddleware(RateLimitConfigs.THEME_SHARE_CREATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ThemeShareBodySchema),
		async (ctx) => {
			const {css} = ctx.req.valid('json');
			const cssBytes = Buffer.from(css, 'utf-8');

			if (cssBytes.length > MAX_CSS_BYTES) {
				throw new FileSizeTooLargeError();
			}

			const themeId = randomBytes(8).toString('hex');
			await ctx.get('storageService').uploadObject({
				bucket: Config.s3.buckets.cdn,
				key: `themes/${themeId}.css`,
				body: cssBytes,
				contentType: 'text/css; charset=utf-8',
			});

			return ctx.json({id: themeId}, 201);
		},
	);
};
