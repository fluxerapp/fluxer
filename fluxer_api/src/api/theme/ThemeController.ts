// SPDX-License-Identifier: AGPL-3.0-or-later

import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {ThemeCreateRequest, ThemeCreateResponse} from '@fluxer/schema/src/domains/theme/ThemeSchemas';

export function ThemeController(app: HonoApp) {
	app.post(
		'/users/@me/themes',
		RateLimitMiddleware(RateLimitConfigs.THEME_SHARE_CREATE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'create_theme',
			summary: 'Create theme',
			responseSchema: ThemeCreateResponse,
			statusCode: 201,
			security: ['bearerToken', 'sessionToken'],
			tags: ['Themes'],
			description: 'Creates a new custom theme with CSS styling that can be shared with other users.',
		}),
		Validator('json', ThemeCreateRequest),
		async (ctx) => {
			const {css} = ctx.req.valid('json');
			const theme = await ctx.get('themeService').createTheme(css);
			return ctx.json(theme, 201);
		},
	);
}
