// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildGifPickerGridData} from '@app/features/channel/components/pickers/gif/GifPickerGridData';
import {bench, describe} from 'vitest';

function gif(index: number, width = 200 + (index % 80), height = 120 + (index % 160)) {
	return {
		id: `gif-${index}`,
		slug: `gif-${index}`,
		provider: 'klipy',
		title: `GIF ${index}`,
		url: `https://example.test/gif-${index}`,
		src: `https://cdn.example.test/gif-${index}.webp`,
		proxy_src: `https://proxy.example.test/gif-${index}.webp`,
		width,
		height,
		media: {},
	};
}

const GIFS = Array.from({length: 500}, (_, index) => gif(index));
const FAVORITE_GIFS = Array.from({length: 500}, (_, index) => ({
	url: `https://example.test/favorite-${index}`,
	proxy_url: `https://proxy.example.test/favorite-${index}.webp`,
	width: 220,
	height: 140 + (index % 120),
	content_type: 'image/gif',
	placeholder: null,
	media: {
		webp: {
			src: `https://cdn.example.test/favorite-${index}.webp`,
			proxy_src: `https://proxy.example.test/favorite-${index}.webp`,
			width: 220,
			height: 140 + (index % 120),
		},
	},
}));
const FEATURED = {
	gifs: [gif(0)],
	categories: Array.from({length: 100}, (_, index) => ({
		name: `category-${index}`,
		src: `https://cdn.example.test/category-${index}.webp`,
		proxy_src: `https://proxy.example.test/category-${index}.webp`,
		gif: index % 3 === 0 ? gif(index) : null,
	})),
};
const FAVORITE_MEMES = Array.from({length: 100}, (_, index) => ({
	contentType: index % 2 === 0 ? 'video/mp4' : 'image/png',
	url: `https://cdn.example.test/meme-${index}.mp4`,
}));

describe('GifPickerGridData benchmarks', () => {
	bench('builds 500 GIF result items', () => {
		buildGifPickerGridData({
			surface: 'results',
			loading: false,
			columns: 4,
			provider: 'klipy',
			featured: FEATURED,
			gifs: GIFS,
			favoriteGifs: FAVORITE_GIFS,
			favoriteMemes: FAVORITE_MEMES,
			useSavedMediaForGifFavorites: false,
			featuredFavoritePreviewSeed: 0.42,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
	});

	bench('builds 500 favorite GIF items', () => {
		buildGifPickerGridData({
			surface: 'favorites',
			loading: false,
			columns: 4,
			provider: 'klipy',
			featured: FEATURED,
			gifs: GIFS,
			favoriteGifs: FAVORITE_GIFS,
			favoriteMemes: FAVORITE_MEMES,
			useSavedMediaForGifFavorites: false,
			featuredFavoritePreviewSeed: 0.42,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
	});

	bench('builds featured categories and preview tiles', () => {
		buildGifPickerGridData({
			surface: 'featured',
			loading: false,
			columns: 4,
			provider: 'klipy',
			featured: FEATURED,
			gifs: GIFS,
			favoriteGifs: FAVORITE_GIFS,
			favoriteMemes: FAVORITE_MEMES,
			useSavedMediaForGifFavorites: true,
			featuredFavoritePreviewSeed: 0.42,
			favoriteTitle: 'Favorites',
			trendingTitle: 'Trending',
		});
	});
});
