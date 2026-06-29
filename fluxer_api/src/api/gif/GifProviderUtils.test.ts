// SPDX-License-Identifier: AGPL-3.0-or-later

import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {describe, expect, it} from 'vitest';
import {tryExtractGifProviderSlug} from './GifProviderUtils';
import type {IGifProvider} from './IGifProvider';

function createProvider(overrides: Partial<IGifProvider> = {}): IGifProvider {
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
		extractSlugFromUrl: () => 'slug',
		...overrides,
	};
}

describe('GifProviderUtils', () => {
	it('returns a trimmed slug from an available provider', async () => {
		await expect(
			tryExtractGifProviderSlug(
				createProvider({
					extractSlugFromUrl: () => '  goatplaybanjo-chat-4  ',
				}),
				'https://klipy.com/gifs/goatplaybanjo-chat-4',
			),
		).resolves.toBe('goatplaybanjo-chat-4');
	});

	it('treats unavailable optional provider failures as no match', async () => {
		await expect(
			tryExtractGifProviderSlug(
				createProvider({
					extractSlugFromUrl: () => {
						throw new ServiceUnavailableError();
					},
				}),
				'https://klipy.com/gifs/goatplaybanjo-chat-4',
			),
		).resolves.toBeNull();
	});

	it('does not check availability when a provider cannot extract a slug', async () => {
		let availabilityChecks = 0;
		await expect(
			tryExtractGifProviderSlug(
				createProvider({
					isAvailable: async () => {
						availabilityChecks += 1;
						return true;
					},
					extractSlugFromUrl: () => null,
				}),
				'https://example.com/media.gif',
			),
		).resolves.toBeNull();

		expect(availabilityChecks).toBe(0);
	});

	it('returns null when the configured provider is unavailable', async () => {
		await expect(
			tryExtractGifProviderSlug(
				createProvider({
					isAvailable: async () => false,
					extractSlugFromUrl: () => 'matched',
				}),
				'https://klipy.com/gifs/matched',
			),
		).resolves.toBeNull();
	});
});
