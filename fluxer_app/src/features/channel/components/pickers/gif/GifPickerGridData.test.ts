// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import type {FavoriteGifEntry} from './FavoriteGifTypes';
import {buildGifPickerGridData} from './GifPickerGridData';
import type {FavoriteAwareGif} from './GifPickerTypes';

function gif(id: string, width = 200, height = 120): FavoriteAwareGif {
	return {
		id,
		slug: id,
		provider: 'klipy',
		title: id,
		url: `https://example.test/${id}`,
		src: `https://cdn.example.test/${id}.webp`,
		proxy_src: `https://proxy.example.test/${id}.webp`,
		width,
		height,
		media: {},
	};
}

const OPAQUE_PROXY_URL =
	'https://media.test/external/sig/v2/aHR0cHM6Ly9zdGF0aWMua2xpcHkuY29tL2lpLzhjLzZhL2E4LzVsU2U1SFBCLndlYm0';

function slimFavorite(overrides: Partial<FavoriteGifEntry> = {}): FavoriteGifEntry {
	return {
		url: 'https://klipy.com/gifs/doc-brown-bttf',
		proxy_url: OPAQUE_PROXY_URL,
		width: 640,
		height: 360,
		media: {},
		content_type: 'video/webm',
		placeholder: null,
		...overrides,
	};
}

describe('GifPickerGridData', () => {
	it('carries the stored content type of a slimmed favorite into its tile', () => {
		const data = buildGifPickerGridData({
			surface: 'favorites',
			loading: false,
			columns: 3,
			provider: 'klipy',
			featured: {gifs: [], categories: []},
			gifs: [],
			favoriteGifs: [slimFavorite()],
			favoriteMemes: [],
			useSavedMediaForGifFavorites: false,
			featuredFavoritePreviewSeed: 0,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
		expect(data[0]).toMatchObject({
			type: 'gif',
			gif: {proxy_src: OPAQUE_PROXY_URL, contentType: 'video/webm'},
		});
	});

	it('carries the stored content type of a slimmed favorite into the featured favorites tile', () => {
		const data = buildGifPickerGridData({
			surface: 'featured',
			loading: false,
			columns: 3,
			provider: 'klipy',
			featured: {gifs: [gif('featured')], categories: []},
			gifs: [],
			favoriteGifs: [slimFavorite()],
			favoriteMemes: [],
			useSavedMediaForGifFavorites: false,
			featuredFavoritePreviewSeed: 0,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
		expect(data[0]).toMatchObject({
			type: 'category',
			previewProxySrc: OPAQUE_PROXY_URL,
			previewContentType: 'video/webm',
		});
	});

	it('takes the content type of the saved media standing in for an empty favorites tile', () => {
		const data = buildGifPickerGridData({
			surface: 'featured',
			loading: false,
			columns: 3,
			provider: 'klipy',
			featured: {gifs: [gif('featured')], categories: []},
			gifs: [],
			favoriteGifs: [],
			favoriteMemes: [{contentType: 'video/mp4', url: 'meme-0'}],
			useSavedMediaForGifFavorites: true,
			featuredFavoritePreviewSeed: 0,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
		expect(data[0]).toMatchObject({type: 'category', previewProxySrc: 'meme-0', previewContentType: 'video/mp4'});
	});

	it('keeps featured data deterministic while choosing preview tiles', () => {
		const data = buildGifPickerGridData({
			surface: 'featured',
			loading: false,
			columns: 3,
			provider: 'klipy',
			featured: {
				gifs: [gif('featured')],
				categories: [{name: 'cats', src: 'cat-src', proxy_src: 'cat-proxy', gif: null}],
			},
			gifs: [],
			favoriteGifs: [],
			favoriteMemes: [
				{contentType: 'video/mp4', url: 'meme-0'},
				{contentType: 'image/png', url: 'ignored'},
				{contentType: 'image/gif', url: 'meme-1'},
			],
			useSavedMediaForGifFavorites: true,
			featuredFavoritePreviewSeed: 0.75,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
		expect(data.map((item) => item.key)).toEqual(['favorites', 'trending', 'cats']);
		expect(data[0]).toMatchObject({type: 'category', previewUrl: 'meme-1', previewProxySrc: 'meme-1'});
		expect(data[1]).toMatchObject({type: 'category', previewUrl: 'https://cdn.example.test/featured.webp'});
	});

	it('omits the favorites tile when includeFavoritesTile is false', () => {
		const data = buildGifPickerGridData({
			surface: 'featured',
			loading: false,
			columns: 3,
			provider: 'klipy',
			featured: {
				gifs: [gif('featured')],
				categories: [{name: 'cats', src: 'cat-src', proxy_src: 'cat-proxy', gif: null}],
			},
			gifs: [],
			favoriteGifs: [],
			favoriteMemes: [],
			useSavedMediaForGifFavorites: false,
			includeFavoritesTile: false,
			featuredFavoritePreviewSeed: 0,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
		expect(data.map((item) => item.key)).toEqual(['trending', 'cats']);
	});

	it('builds skeletons only when result data is empty during loading', () => {
		const data = buildGifPickerGridData({
			surface: 'results',
			loading: true,
			columns: 4,
			provider: 'klipy',
			featured: {gifs: [], categories: []},
			gifs: [],
			favoriteGifs: [],
			favoriteMemes: [],
			useSavedMediaForGifFavorites: false,
			featuredFavoritePreviewSeed: 0,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
		expect(data).toHaveLength(12);
		expect(data.every((item) => item.type === 'skeleton')).toBe(true);
	});

	it('keeps old result items instead of skeletonizing while a newer query is loading', () => {
		const data = buildGifPickerGridData({
			surface: 'results',
			loading: true,
			columns: 4,
			provider: 'klipy',
			featured: {gifs: [], categories: []},
			gifs: [gif('old-result')],
			favoriteGifs: [],
			favoriteMemes: [],
			useSavedMediaForGifFavorites: false,
			featuredFavoritePreviewSeed: 0,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
		expect(data).toEqual([{type: 'gif', key: 'old-result', gif: gif('old-result')}]);
	});
});
