// SPDX-License-Identifier: AGPL-3.0-or-later

import {type NativeImage, nativeImage} from 'electron';

export type TrayIconAppearance = 'colored' | 'template' | 'white' | 'black';

type Bgr = Readonly<[number, number, number]>;

const DOT_RADIUS_RATIO = 0.22;
const DOT_GAP_RATIO = 0.0625;
const SUBSAMPLES = 4;
const MIN_GLYPH_CONTRAST = 32;
const GLYPH_CUTOUT_START = 0.1;
const GLYPH_CUTOUT_END = 0.5;
const DOT_BGR: Bgr = [0x00, 0x00, 0xdd];
const TEMPLATE_DOT_BGR: Bgr = [0x00, 0x00, 0x00];
const GLYPH_BGR: Readonly<Record<'white' | 'black', Bgr>> = {
	white: [0xff, 0xff, 0xff],
	black: [0x1c, 0x1c, 0x1c],
};

export function resolveTrayIconAppearance(options: {
	platform: NodeJS.Platform;
	coloredTrayIcon: boolean;
	darkSystemUI: boolean;
}): TrayIconAppearance {
	if (options.platform === 'darwin') return 'template';
	if (options.coloredTrayIcon) return 'colored';
	if (options.platform === 'win32' && !options.darkSystemUI) return 'black';
	return 'white';
}

function coverage(x: number, y: number, centerX: number, centerY: number, radius: number): number {
	let hits = 0;
	for (let sy = 0; sy < SUBSAMPLES; sy++) {
		const dy = y + (sy + 0.5) / SUBSAMPLES - centerY;
		for (let sx = 0; sx < SUBSAMPLES; sx++) {
			const dx = x + (sx + 0.5) / SUBSAMPLES - centerX;
			if (dx * dx + dy * dy <= radius * radius) {
				hits++;
			}
		}
	}
	return hits / (SUBSAMPLES * SUBSAMPLES);
}

function luminance(bitmap: Buffer, offset: number): number {
	const alpha = bitmap[offset + 3];
	if (alpha === 0) return 0;
	return (0.0722 * bitmap[offset] + 0.7152 * bitmap[offset + 1] + 0.2126 * bitmap[offset + 2]) * (255 / alpha);
}

function bodyLuminance(bitmap: Buffer): number | null {
	const histogram = new Array<number>(256).fill(0);
	let opaque = 0;
	for (let offset = 0; offset < bitmap.length; offset += 4) {
		if (bitmap[offset + 3] !== 255) continue;
		histogram[Math.min(255, Math.round(luminance(bitmap, offset)))]++;
		opaque++;
	}
	let seen = 0;
	for (let level = 0; level < 256; level++) {
		seen += histogram[level];
		if (seen * 2 >= opaque && opaque > 0) return level;
	}
	return null;
}

export function paintMonochromeGlyph(bitmap: Buffer, glyphBgr: Bgr): void {
	const body = bodyLuminance(bitmap);
	let highlight = 0;
	for (let offset = 0; offset < bitmap.length; offset += 4) {
		if (bitmap[offset + 3] >= 128) {
			highlight = Math.max(highlight, luminance(bitmap, offset));
		}
	}
	const contrast = body === null ? 0 : highlight - body;
	for (let offset = 0; offset < bitmap.length; offset += 4) {
		let alpha = bitmap[offset + 3];
		if (body !== null && contrast >= MIN_GLYPH_CONTRAST) {
			const position = (luminance(bitmap, offset) - body) / contrast;
			const cutout = Math.min(
				1,
				Math.max(0, (position - GLYPH_CUTOUT_START) / (GLYPH_CUTOUT_END - GLYPH_CUTOUT_START)),
			);
			alpha = Math.round(alpha * (1 - cutout));
		}
		for (let channel = 0; channel < 3; channel++) {
			bitmap[offset + channel] = Math.round((glyphBgr[channel] * alpha) / 255);
		}
		bitmap[offset + 3] = alpha;
	}
}

export function paintTrayIndicatorDot(bitmap: Buffer, width: number, height: number, dotBgr: Bgr): void {
	const size = Math.min(width, height);
	const radius = size * DOT_RADIUS_RATIO;
	const cutRadius = radius + size * DOT_GAP_RATIO;
	const centerX = width - radius;
	const centerY = radius;
	const minX = Math.max(0, Math.floor(centerX - cutRadius));
	const maxY = Math.min(height, Math.ceil(centerY + cutRadius));
	for (let y = 0; y < maxY; y++) {
		for (let x = minX; x < width; x++) {
			const cut = coverage(x, y, centerX, centerY, cutRadius);
			if (cut === 0) continue;
			const dot = coverage(x, y, centerX, centerY, radius);
			const offset = (y * width + x) * 4;
			for (let channel = 0; channel < 3; channel++) {
				bitmap[offset + channel] = Math.round(bitmap[offset + channel] * (1 - cut) + dotBgr[channel] * dot);
			}
			bitmap[offset + 3] = Math.round(bitmap[offset + 3] * (1 - cut) + 255 * dot);
		}
	}
}

export function renderTrayIcon(base: NativeImage, appearance: TrayIconAppearance, indicator: boolean): NativeImage {
	const monochrome = appearance === 'white' || appearance === 'black';
	if (!monochrome && !indicator) {
		return base;
	}
	const result = nativeImage.createEmpty();
	const scaleFactors = base.getScaleFactors();
	for (const scaleFactor of scaleFactors.length > 0 ? scaleFactors : [1]) {
		const size = base.getSize(scaleFactor);
		const width = Math.round(size.width * scaleFactor);
		const height = Math.round(size.height * scaleFactor);
		const bitmap = Buffer.from(base.toBitmap({scaleFactor}));
		if (width === 0 || height === 0 || bitmap.length !== width * height * 4) continue;
		if (monochrome) {
			paintMonochromeGlyph(bitmap, GLYPH_BGR[appearance]);
		}
		if (indicator) {
			paintTrayIndicatorDot(bitmap, width, height, appearance === 'template' ? TEMPLATE_DOT_BGR : DOT_BGR);
		}
		result.addRepresentation({scaleFactor, width, height, buffer: bitmap});
	}
	if (result.isEmpty()) {
		return base;
	}
	result.setTemplateImage(appearance === 'template');
	return result;
}
