// SPDX-License-Identifier: AGPL-3.0-or-later

import {GIF_PROVIDER_HEADER, type GifResponse} from '@fluxer/schema/src/domains/gif/GifSchemas';
import {Hono} from 'hono';
import {describe, expect, it, vi} from 'vitest';
import type {HonoEnv} from '../types/HonoEnv';
import {GifController} from './GifController';
import {GifService} from './GifService';
import type {IGifProvider} from './IGifProvider';

function createProvider(gifs: Array<GifResponse>): IGifProvider {
	return {
		meta: {
			name: 'klipy',
			displayName: 'KLIPY',
			attributionRequired: true,
		},
		isAvailable: async () => true,
		search: vi.fn(async () => gifs),
		registerShare: async () => undefined,
		getFeatured: async () => ({gifs: [], categories: []}),
		getTrendingGifs: async () => [],
		suggest: async () => [],
		resolveByUrl: async () => null,
		buildShareUrl: (slug) => `https://klipy.com/gifs/${slug}`,
		extractSlugFromUrl: () => null,
	};
}

function createApp(gifService: GifService): Hono<HonoEnv> {
	const app = new Hono<HonoEnv>({strict: true});
	app.use('*', async (ctx, next) => {
		ctx.set('gifService', gifService);
		ctx.set('user', {
			isBot: false,
			suspiciousActivityFlags: 0,
		} as HonoEnv['Variables']['user']);
		ctx.set('authTokenType', 'session');
		await next();
	});
	GifController(app);
	return app;
}

describe('GifController', () => {
	it('serves deprecated Tenor routes from the configured KLIPY provider', async () => {
		const gifs: Array<GifResponse> = [
			{
				id: 'goatplaybanjo-chat-4',
				slug: 'goatplaybanjo-chat-4',
				provider: 'klipy',
				title: 'Goatplaybanjo Chat',
				url: 'https://klipy.com/gifs/goatplaybanjo-chat-4',
				src: 'https://static.klipy.example/full.webm',
				proxy_src: 'https://media.example/full.webm',
				width: 220,
				height: 229,
				media: {},
				placeholder: null,
			},
		];
		const provider = createProvider(gifs);
		const response = await createApp(new GifService(provider)).request('/tenor/search?q=cat&locale=en-US');

		expect(response.status).toBe(200);
		expect(response.headers.get('Deprecation')).toBe('true');
		expect(response.headers.get(GIF_PROVIDER_HEADER)).toBe('klipy');
		await expect(response.json()).resolves.toEqual(gifs);
		expect(provider.search).toHaveBeenCalledWith({q: 'cat', locale: 'en_US', country: 'US'});
	});
});
