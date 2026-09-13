// SPDX-License-Identifier: AGPL-3.0-or-later

import {isEmbeddableImageFile} from '@app/features/messaging/utils/EmbeddableImageTypes';
import {describe, expect, it} from 'vitest';

describe('isEmbeddableImageFile', () => {
	it('embeds common raster image mime types', () => {
		expect(isEmbeddableImageFile({type: 'image/png', name: 'a.png'})).toBe(true);
		expect(isEmbeddableImageFile({type: 'image/jpeg', name: 'a.jpg'})).toBe(true);
		expect(isEmbeddableImageFile({type: 'image/gif', name: 'a.gif'})).toBe(true);
		expect(isEmbeddableImageFile({type: 'image/webp', name: 'a.webp'})).toBe(true);
		expect(isEmbeddableImageFile({type: 'image/avif', name: 'a.avif'})).toBe(true);
	});
	it('embeds SVG files (media proxy sanitizes them server-side)', () => {
		expect(isEmbeddableImageFile({type: 'image/svg+xml', name: 'logo.svg'})).toBe(true);
		expect(isEmbeddableImageFile({type: 'image/svg', name: 'logo.svg'})).toBe(true);
		expect(isEmbeddableImageFile({type: '', name: 'logo.svg'})).toBe(true);
		expect(isEmbeddableImageFile({type: 'image/svg+xml; charset=utf-8', name: 'logo.svg'})).toBe(true);
	});
	it('rejects non-image content types with no embeddable extension', () => {
		expect(isEmbeddableImageFile({type: 'application/pdf', name: 'doc.pdf'})).toBe(false);
		expect(isEmbeddableImageFile({type: 'text/html', name: 'page.html'})).toBe(false);
		expect(isEmbeddableImageFile({type: 'application/octet-stream', name: 'blob.bin'})).toBe(false);
	});
	it('falls back to the filename extension when the mime is missing', () => {
		expect(isEmbeddableImageFile({type: '', name: 'photo.png'})).toBe(true);
		expect(isEmbeddableImageFile({type: '', name: 'photo.unknown'})).toBe(false);
	});
});
