// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getImageUploadMimeType,
	isSvgFile,
	isSvgMimeType,
	normalizeImageMimeType,
} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {describe, expect, it} from 'vitest';

describe('ImageUploadFileUtils', () => {
	it('normalizes image MIME type parameters', () => {
		expect(normalizeImageMimeType('IMAGE/SVG+XML; charset=utf-8')).toBe('image/svg+xml');
	});
	it('detects SVG files by MIME type or filename', () => {
		expect(isSvgMimeType('image/svg+xml; charset=utf-8')).toBe(true);
		expect(isSvgFile({type: 'image/svg', name: 'vector'})).toBe(true);
		expect(isSvgFile({type: '', name: 'vector.SVG'})).toBe(true);
		expect(isSvgFile({type: 'image/png', name: 'vector.png'})).toBe(false);
	});
	it('uses a canonical SVG MIME type for uploads', () => {
		expect(getImageUploadMimeType({type: 'image/svg', name: 'vector'})).toBe('image/svg+xml');
		expect(getImageUploadMimeType({type: 'image/webp; charset=utf-8', name: 'image.webp'})).toBe('image/webp');
	});
});
