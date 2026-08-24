// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type FavoriteGifEntry,
	pickBestPreviewFormat,
} from '@app/features/channel/components/pickers/gif/FavoriteGifTypes';
import type {GifPickerGridItemData} from '@app/features/channel/components/pickers/gif/GifPickerTypes';
import type {Gif, GifFeatured} from '@app/features/expressions/commands/GifCommands';
import * as GifSlugUtils from '@app/features/expressions/utils/GifSlugUtils';

const CATEGORY_TILE_WIDTH = 200;
const CATEGORY_TILE_HEIGHT = 96;
const DEFAULT_GIF_SIZE = 200;

export interface GifPickerFavoriteMemePreview {
	contentType: string;
	url: string;
}

export interface BuildGifPickerGridDataInput {
	surface: 'favorites' | 'featured' | 'results';
	loading: boolean;
	columns: number;
	provider: string;
	featured: GifFeatured;
	gifs: ReadonlyArray<Gif>;
	favoriteGifs: ReadonlyArray<FavoriteGifEntry>;
	favoriteMemes: ReadonlyArray<GifPickerFavoriteMemePreview>;
	useSavedMediaForGifFavorites: boolean;
	includeFavoritesTile?: boolean;
	featuredFavoritePreviewSeed: number;
	favoriteTitle: string;
	trendingTitle: string;
}

export function buildSkeletonGifPickerItems(count: number): Array<GifPickerGridItemData> {
	return Array.from({length: count}, (_, i) => ({
		type: 'skeleton',
		key: `skeleton-${i}`,
		width: DEFAULT_GIF_SIZE,
		height: DEFAULT_GIF_SIZE,
	}));
}

export function buildGifPickerGridData(input: BuildGifPickerGridDataInput): Array<GifPickerGridItemData> {
	if (input.surface === 'favorites') {
		return buildFavoriteGifItems(input.provider, input.favoriteGifs);
	}
	if (input.surface === 'featured') {
		return buildFeaturedItems(input);
	}
	if (input.loading && input.gifs.length === 0) {
		return buildSkeletonGifPickerItems(Math.max(input.columns * 3, 12));
	}
	return input.gifs.map((gif) => ({
		type: 'gif' as const,
		key: gif.id || gif.src,
		gif,
	}));
}

function buildFavoriteGifItems(
	provider: string,
	favoriteGifs: ReadonlyArray<FavoriteGifEntry>,
): Array<GifPickerGridItemData> {
	const items: Array<GifPickerGridItemData> = [];
	for (let index = favoriteGifs.length - 1; index >= 0; index -= 1) {
		const entry = favoriteGifs[index];
		const best = pickBestPreviewFormat(entry.media);
		const fallbackSrc = GifSlugUtils.isUsableMediaSource(entry.proxy_url) ? entry.proxy_url : '';
		const previewSrc = best?.format.src ?? fallbackSrc;
		const previewProxySrc = best?.format.proxy_src ?? fallbackSrc;
		const previewWidth = best?.format.width ?? entry.width;
		const previewHeight = best?.format.height ?? entry.height;
		items.push({
			type: 'gif',
			key: entry.url,
			gif: {
				id: entry.url,
				slug: '',
				provider,
				title: 'GIF',
				url: entry.url,
				src: previewSrc,
				proxy_src: previewProxySrc,
				width: previewWidth > 0 ? previewWidth : DEFAULT_GIF_SIZE,
				height: previewHeight > 0 ? previewHeight : DEFAULT_GIF_SIZE,
				media: entry.media,
				favoriteGifLookup: {url: entry.url},
			},
		});
	}
	return items;
}

function buildFeaturedItems(input: BuildGifPickerGridDataInput): Array<GifPickerGridItemData> {
	const gifvMemes = input.favoriteMemes.filter(
		(meme) => meme.contentType.includes('gif') || meme.contentType.startsWith('video/'),
	);
	const favoriteGifPreviewIndex =
		input.favoriteGifs.length > 0 ? Math.floor(input.featuredFavoritePreviewSeed * input.favoriteGifs.length) : -1;
	const favoriteGifPreviewEntry =
		favoriteGifPreviewIndex >= 0 ? input.favoriteGifs[favoriteGifPreviewIndex] : undefined;
	const favoriteGifPreview = pickBestPreviewFormat(favoriteGifPreviewEntry?.media);
	const favoriteMemePreview =
		gifvMemes.length > 0
			? (gifvMemes[Math.floor(input.featuredFavoritePreviewSeed * gifvMemes.length)]?.url ?? '')
			: '';
	const favoriteTilePreview =
		input.useSavedMediaForGifFavorites && input.favoriteGifs.length === 0
			? favoriteMemePreview
			: favoriteGifPreview?.format.src || favoriteGifPreviewEntry?.proxy_url || favoriteGifPreviewEntry?.url || '';
	const favoriteTileProxyPreview =
		input.useSavedMediaForGifFavorites && input.favoriteGifs.length === 0
			? favoriteTilePreview
			: favoriteGifPreview?.format.proxy_src ||
				favoriteGifPreviewEntry?.proxy_url ||
				favoriteGifPreviewEntry?.url ||
				'';
	const favoritesTile: Array<GifPickerGridItemData> =
		(input.includeFavoritesTile ?? true)
			? [
					{
						type: 'category',
						categoryKind: 'favorites',
						key: 'favorites',
						id: 'favorites',
						title: input.favoriteTitle,
						previewUrl: favoriteTilePreview,
						previewProxySrc: favoriteTileProxyPreview,
						width: CATEGORY_TILE_WIDTH,
						height: CATEGORY_TILE_HEIGHT,
					},
				]
			: [];
	const trendingGif = input.featured.gifs[0];
	const trendingPreview = pickBestPreviewFormat(trendingGif?.media);
	return [
		...favoritesTile,
		{
			type: 'category',
			categoryKind: 'trending',
			key: 'trending',
			id: 'trending',
			title: input.trendingTitle,
			previewUrl: trendingPreview?.format.src ?? trendingGif?.src ?? trendingGif?.url ?? '',
			previewProxySrc: trendingPreview?.format.proxy_src ?? trendingGif?.proxy_src ?? trendingGif?.src ?? '',
			width: CATEGORY_TILE_WIDTH,
			height: CATEGORY_TILE_HEIGHT,
		},
		...input.featured.categories.map((category) => {
			const categoryPreview = pickBestPreviewFormat(category.gif?.media);
			return {
				type: 'category' as const,
				categoryKind: 'category' as const,
				key: category.name,
				id: category.name,
				title: category.name,
				previewUrl: categoryPreview?.format.src ?? category.gif?.src ?? category.src,
				previewProxySrc:
					categoryPreview?.format.proxy_src ?? category.gif?.proxy_src ?? category.proxy_src ?? category.src,
				width: CATEGORY_TILE_WIDTH,
				height: CATEGORY_TILE_HEIGHT,
			};
		}),
	];
}
