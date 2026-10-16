// SPDX-License-Identifier: AGPL-3.0-or-later

import {LRUCache} from 'lru-cache';

const RETAINED_IMAGE_BYTE_BUDGET = 192 * 1024 * 1024;
const RETAINED_IMAGE_MAX_ENTRIES = 2000;
const RETAINED_IMAGE_MIN_BYTES = 1024;
const BYTES_PER_PIXEL = 4;
const MAX_RETAINED_SOURCE_LENGTH = 16 * 1024;

const retainedImages = new LRUCache<string, HTMLImageElement>({
	max: RETAINED_IMAGE_MAX_ENTRIES,
	maxSize: RETAINED_IMAGE_BYTE_BUDGET,
	sizeCalculation: (image) => estimateDecodedBytes(image.naturalWidth, image.naturalHeight),
});

function estimateDecodedBytes(width: number, height: number): number {
	return Math.min(RETAINED_IMAGE_BYTE_BUDGET, Math.max(RETAINED_IMAGE_MIN_BYTES, width * height * BYTES_PER_PIXEL));
}

function isRetainableSource(src: string): boolean {
	return (
		src.length > 0 && src.length <= MAX_RETAINED_SOURCE_LENGTH && !src.startsWith('data:') && !src.startsWith('blob:')
	);
}

export function retainImage(image: HTMLImageElement): void {
	if (!image.complete || image.naturalWidth <= 0) return;
	const src = image.currentSrc || image.src;
	if (!isRetainableSource(src)) return;
	if (retainedImages.get(src) != null) return;
	const holder = new Image();
	holder.decoding = 'async';
	holder.src = src;
	retainedImages.set(src, holder, {size: estimateDecodedBytes(image.naturalWidth, image.naturalHeight)});
}

export function installImageRetention(target: Document): () => void {
	const handleLoad = (event: Event): void => {
		if (event.target instanceof HTMLImageElement) retainImage(event.target);
	};
	target.addEventListener('load', handleLoad, true);
	return () => target.removeEventListener('load', handleLoad, true);
}
