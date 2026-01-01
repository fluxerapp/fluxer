/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {LRUCache} from 'lru-cache';

interface ImageCacheEntry {
	loaded: boolean;
}

const imageCache = new LRUCache<string, ImageCacheEntry>({
	max: 500,
	ttl: 1000 * 60 * 10,
});

const isCached = (src: string | null): boolean => {
	if (!src) return false;
	return imageCache.has(src);
};

export const hasImage = (src: string | null): boolean => {
	return isCached(src);
};

export const loadImage = (src: string | null, onLoad: () => void, onError?: () => void): void => {
	if (!src) {
		onError?.();
		return;
	}

	if (imageCache.has(src)) {
		onLoad();
		return;
	}

	const image = new Image();

	image.onload = () => {
		imageCache.set(src, {loaded: true});
		onLoad();
	};

	image.onerror = () => {
		onError?.();
	};

	image.src = src;
};
