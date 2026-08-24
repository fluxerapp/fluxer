// SPDX-License-Identifier: AGPL-3.0-or-later

import {inferFormatContentType, PREVIEW_FORMAT_PRIORITY} from '@fluxer/schema/src/domains/gif/GifMediaFormatKeys';

export interface FavoriteGifMediaFormat {
	src: string;
	proxy_src: string;
	width: number;
	height: number;
}

export interface FavoriteGifEntry {
	url: string;
	proxy_url: string;
	width: number;
	height: number;
	media: Record<string, FavoriteGifMediaFormat>;
	content_type: string;
	placeholder: string | null;
}

export const PREVIEW_TILE_CSS_WIDTH = 200;

const PLAYABLE_CLASS = 0;
const STILL_CLASS = 1;
const UNKNOWN_CLASS = 2;

interface PreviewCandidate {
	key: string;
	format: FavoriteGifMediaFormat;
	formatClass: number;
	priority: number;
}

export interface PreviewTileBox {
	cssWidth?: number;
	devicePixelRatio?: number;
}

function isUsablePreviewFormat(format: FavoriteGifMediaFormat | undefined): format is FavoriteGifMediaFormat {
	return Boolean(format?.src && format.proxy_src && format.width > 0 && format.height > 0);
}

function previewFormatClass(formatKey: string): number {
	const contentType = inferFormatContentType(formatKey);
	if (contentType.startsWith('video/')) return PLAYABLE_CLASS;
	if (contentType.startsWith('image/')) return STILL_CLASS;
	return UNKNOWN_CLASS;
}

function previewFormatPriority(formatKey: string): number {
	const index = PREVIEW_FORMAT_PRIORITY.indexOf(formatKey as (typeof PREVIEW_FORMAT_PRIORITY)[number]);
	return index === -1 ? PREVIEW_FORMAT_PRIORITY.length : index;
}

function currentDevicePixelRatio(): number {
	const ratio = (globalThis as {devicePixelRatio?: number}).devicePixelRatio;
	return typeof ratio === 'number' && Number.isFinite(ratio) && ratio > 0 ? ratio : 1;
}

export function previewTileDeviceWidth(tile: PreviewTileBox = {}): number {
	return (tile.cssWidth ?? PREVIEW_TILE_CSS_WIDTH) * (tile.devicePixelRatio ?? currentDevicePixelRatio());
}

export function pickBestPreviewFormat(
	media: Record<string, FavoriteGifMediaFormat> | null | undefined,
	kind: 'any' | 'image' = 'any',
	tile: PreviewTileBox = {},
): {key: string; format: FavoriteGifMediaFormat} | null {
	if (!media) return null;
	const deviceWidth = previewTileDeviceWidth(tile);
	const candidates: Array<PreviewCandidate> = [];
	for (const [key, format] of Object.entries(media)) {
		if (!isUsablePreviewFormat(format)) continue;
		const formatClass = previewFormatClass(key);
		if (kind === 'image' && formatClass !== STILL_CLASS) continue;
		candidates.push({key, format, formatClass, priority: previewFormatPriority(key)});
	}
	if (candidates.length === 0) return null;
	const filling = candidates.filter((candidate) => candidate.format.width >= deviceWidth);
	const pool = filling.length > 0 ? filling : candidates;
	const widthDirection = filling.length > 0 ? 1 : -1;
	pool.sort(
		(a, b) =>
			a.formatClass - b.formatClass ||
			widthDirection * (a.format.width - b.format.width) ||
			a.priority - b.priority ||
			a.key.localeCompare(b.key),
	);
	const best = pool[0];
	return {key: best.key, format: best.format};
}

export function pickCanonicalPreviewFormat(
	media: Record<string, FavoriteGifMediaFormat> | null | undefined,
): {key: string; format: FavoriteGifMediaFormat} | null {
	if (!media) return null;
	for (const key of PREVIEW_FORMAT_PRIORITY) {
		const format = media[key];
		if (isUsablePreviewFormat(format)) return {key, format};
	}
	for (const [key, format] of Object.entries(media)) {
		if (isUsablePreviewFormat(format)) return {key, format};
	}
	return null;
}

export {inferFormatContentType};
