// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {InvalidApiOriginError} from '@fluxer/errors/src/domains/core/InvalidApiOriginError';
import type {Context, Next} from 'hono';

const LEGACY_APP_ORIGINS = ['https://web.fluxer.app', 'https://web.canary.fluxer.app'];

export async function BlockAppOriginMiddleware(ctx: Context, next: Next) {
	const origin = ctx.req.header('origin');
	if (
		origin !== undefined &&
		(LEGACY_APP_ORIGINS.includes(origin) || Config.endpoints.webAppOrigins.includes(origin))
	) {
		throw new InvalidApiOriginError();
	}
	await next();
}
