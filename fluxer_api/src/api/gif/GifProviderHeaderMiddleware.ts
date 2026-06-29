// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GIF_PROVIDER_ATTRIBUTION_HEADER,
	GIF_PROVIDER_DISPLAY_NAME_HEADER,
	GIF_PROVIDER_HEADER,
} from '@fluxer/schema/src/domains/gif/GifSchemas';
import {createMiddleware} from 'hono/factory';
import type {HonoEnv} from '../types/HonoEnv';
import type {GifService} from './GifService';

export const GifProviderHeaderMiddleware = createMiddleware<HonoEnv>(async (ctx, next) => {
	await next();
	const gifService = ctx.get('gifService') as GifService | undefined;
	if (!gifService) return;
	const provider = await gifService.getActive().catch(() => null);
	if (!provider) return;
	ctx.header(GIF_PROVIDER_HEADER, provider.meta.name);
	ctx.header(GIF_PROVIDER_DISPLAY_NAME_HEADER, provider.meta.displayName);
	ctx.header(GIF_PROVIDER_ATTRIBUTION_HEADER, provider.meta.attributionRequired ? 'true' : 'false');
});
