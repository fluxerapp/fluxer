// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GifResponse} from '@fluxer/schema/src/domains/gif/GifSchemas';
import {describe, expect, it, vi} from 'vitest';
import {GifService} from '../gif/GifService';
import type {IGifProvider} from '../gif/IGifProvider';
import type {IMediaService} from '../infrastructure/IMediaService';
import type {IUnfurlerService} from '../infrastructure/IUnfurlerService';
import {resolveFavoriteGifEntry} from './FavoriteGifResolver';

function createProvider(gif: GifResponse): IGifProvider {
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
		resolveByUrl: async () => gif,
		buildShareUrl: (slug) => `https://klipy.com/gifs/${slug}`,
		extractSlugFromUrl: () => gif.slug,
	};
}

describe('resolveFavoriteGifEntry', () => {
	it('returns provider GIF media without probing generic media fallbacks', async () => {
		const gif: GifResponse = {
			id: 'goatplaybanjo-chat-4',
			slug: 'goatplaybanjo-chat-4',
			provider: 'klipy',
			title: 'Goatplaybanjo Chat',
			url: 'https://klipy.com/gifs/goatplaybanjo-chat-4',
			src: 'https://static.klipy.example/fallback.gif',
			proxy_src: 'https://media.example/fallback.gif',
			width: 120,
			height: 100,
			media: {
				tinygif: {
					src: 'https://static.klipy.example/tiny.gif',
					proxy_src: 'https://media.example/tiny.gif',
					width: 80,
					height: 60,
				},
				webm: {
					src: 'https://static.klipy.example/full.webm',
					proxy_src: 'https://media.example/full.webm',
					width: 220,
					height: 229,
				},
			},
			placeholder: 'thumbhash',
		};
		const mediaService = {
			getMetadata: vi.fn(),
			getExternalMediaProxyURL: vi.fn(),
		} as unknown as IMediaService;
		const unfurlerService = {
			unfurl: vi.fn(),
		} as unknown as IUnfurlerService;

		await expect(
			resolveFavoriteGifEntry({
				url: gif.url,
				locale: 'en-US',
				country: 'US',
				gifService: new GifService(createProvider(gif)),
				mediaService,
				unfurlerService,
			}),
		).resolves.toEqual({
			url: gif.url,
			proxy_url: 'https://media.example/full.webm',
			width: 220,
			height: 229,
			media: gif.media,
			content_type: 'video/webm',
			placeholder: 'thumbhash',
		});
		expect(mediaService.getMetadata).not.toHaveBeenCalled();
		expect(unfurlerService.unfurl).not.toHaveBeenCalled();
	});
});
