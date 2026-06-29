// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	GIF_PROVIDER_ATTRIBUTION_HEADER,
	GIF_PROVIDER_DISPLAY_NAME_HEADER,
	GIF_PROVIDER_HEADER,
} from '@fluxer/schema/src/domains/gif/GifSchemas';
import {Hono} from 'hono';
import {describe, expect, it} from 'vitest';
import type {HonoEnv} from '../types/HonoEnv';
import {GifProviderHeaderMiddleware} from './GifProviderHeaderMiddleware';
import {GifService} from './GifService';
import type {IGifProvider} from './IGifProvider';

function createProvider(): IGifProvider {
	return {
		meta: {
			name: 'klipy',
			displayName: 'KLIPY',
			attributionRequired: true,
		},
		isAvailable: async () => true,
		search: async () => [],
		registerShare: async () => undefined,
		getFeatured: async () => ({gifs: [], categories: []}),
		getTrendingGifs: async () => [],
		suggest: async () => [],
		resolveByUrl: async () => null,
		buildShareUrl: (slug) => `https://klipy.example/${slug}`,
		extractSlugFromUrl: () => null,
	};
}

function createApp(gifService?: GifService): Hono<HonoEnv> {
	const app = new Hono<HonoEnv>({strict: true});
	if (gifService) {
		app.use('*', async (ctx, next) => {
			ctx.set('gifService', gifService);
			await next();
		});
	}
	app.use('*', GifProviderHeaderMiddleware);
	app.get('/probe', (ctx) => ctx.text('ok'));
	return app;
}

describe('GifProviderHeaderMiddleware', () => {
	it('emits active GIF provider metadata headers', async () => {
		const gifService = new GifService(createProvider());
		const response = await createApp(gifService).request('/probe');

		expect(response.headers.get(GIF_PROVIDER_HEADER)).toBe('klipy');
		expect(response.headers.get(GIF_PROVIDER_DISPLAY_NAME_HEADER)).toBe('KLIPY');
		expect(response.headers.get(GIF_PROVIDER_ATTRIBUTION_HEADER)).toBe('true');
	});

	it('does not emit GIF provider headers when the service is unavailable', async () => {
		const response = await createApp().request('/probe');

		expect(response.headers.has(GIF_PROVIDER_HEADER)).toBe(false);
		expect(response.headers.has(GIF_PROVIDER_DISPLAY_NAME_HEADER)).toBe(false);
		expect(response.headers.has(GIF_PROVIDER_ATTRIBUTION_HEADER)).toBe(false);
	});
});
