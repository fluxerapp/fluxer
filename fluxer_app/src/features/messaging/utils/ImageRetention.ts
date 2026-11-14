// SPDX-License-Identifier: AGPL-3.0-or-later

import {unwrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopLocalResourceTarget';
import {LRUCache} from 'lru-cache';

const RETAINED_IMAGE_BYTE_BUDGET = 192 * 1024 * 1024;
const RETAINED_IMAGE_MAX_ENTRIES = 2000;
const RETAINED_VECTOR_IMAGE_MAX_ENTRIES = 48;
const RETAINED_IMAGE_MAX_PIXELS = 2048 * 2048;
const RETAINED_IMAGE_MIN_BYTES = 1024;
const BYTES_PER_PIXEL = 4;
const MAX_RETAINED_SOURCE_LENGTH = 16 * 1024;

const retainedImages = new LRUCache<string, HTMLImageElement>({
	max: RETAINED_IMAGE_MAX_ENTRIES,
	maxSize: RETAINED_IMAGE_BYTE_BUDGET,
	sizeCalculation: (image) => estimateDecodedBytes(image.naturalWidth, image.naturalHeight),
});

const retainedVectorImages = new LRUCache<string, HTMLImageElement>({max: RETAINED_VECTOR_IMAGE_MAX_ENTRIES});

type RetentionKind = 'raster' | 'vector' | 'none';

function estimateDecodedBytes(width: number, height: number): number {
	return Math.min(RETAINED_IMAGE_BYTE_BUDGET, Math.max(RETAINED_IMAGE_MIN_BYTES, width * height * BYTES_PER_PIXEL));
}

function classifySource(src: string): RetentionKind {
	if (src.length === 0 || src.length > MAX_RETAINED_SOURCE_LENGTH) return 'none';
	if (src.startsWith('data:') || src.startsWith('blob:')) return 'none';
	let url: URL;
	try {
		url = new URL(unwrapDesktopLocalResourceURL(src));
	} catch {
		return 'none';
	}
	if (url.searchParams.get('animated') === 'true') return 'none';
	const path = url.pathname.toLowerCase();
	if (path.endsWith('.gif') && !url.searchParams.has('format')) return 'none';
	return path.endsWith('.svg') ? 'vector' : 'raster';
}

function createHolder(src: string): HTMLImageElement {
	const holder = new Image();
	holder.decoding = 'async';
	holder.src = src;
	return holder;
}

export function retainImage(image: HTMLImageElement): void {
	if (!image.complete || image.naturalWidth <= 0) return;
	const src = image.currentSrc || image.src;
	const kind = classifySource(src);
	if (kind === 'vector') {
		if (retainedVectorImages.get(src) == null) retainedVectorImages.set(src, createHolder(src));
		return;
	}
	if (kind === 'none' || image.naturalWidth * image.naturalHeight > RETAINED_IMAGE_MAX_PIXELS) return;
	if (retainedImages.get(src) != null) return;
	retainedImages.set(src, createHolder(src), {size: estimateDecodedBytes(image.naturalWidth, image.naturalHeight)});
}

export function installImageRetention(target: Document): () => void {
	const handleLoad = (event: Event): void => {
		if (event.target instanceof HTMLImageElement) retainImage(event.target);
	};
	target.addEventListener('load', handleLoad, true);
	return () => target.removeEventListener('load', handleLoad, true);
}
