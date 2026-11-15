// SPDX-License-Identifier: AGPL-3.0-or-later

import {type NativeImage, nativeImage} from 'electron';

export type TrayIconAppearance = 'colored' | 'template' | 'white' | 'black';

type Bgr = Readonly<[number, number, number]>;
interface TrayBitmap {
	buffer: Buffer;
	width: number;
	height: number;
}

const DOT_RADIUS_RATIO = 0.22;
const DOT_GAP_RATIO = 0.0625;
const SUBSAMPLES = 4;
const MIN_GLYPH_CONTRAST = 32;
const GLYPH_CUTOUT_START = 0.1;
const GLYPH_CUTOUT_END = 0.5;
const LINUX_MONOCHROME_CANVAS_RATIO = 22 / 16;
const LINUX_MAX_PIXMAP_SIZE = 256;
const PNG_SIGNATURE = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]);
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

function paintMonochromeGlyph(bitmap: Buffer, glyphBgr: Bgr): void {
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

function paintTrayIndicatorDot(bitmap: Buffer, width: number, height: number, dotBgr: Bgr): void {
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

function cropTrayGlyph(buffer: Buffer, width: number, height: number): TrayBitmap | null {
	let left = width;
	let top = height;
	let right = 0;
	let bottom = 0;
	for (let y = 0; y < height; y++) {
		for (let x = 0; x < width; x++) {
			if (buffer[(y * width + x) * 4 + 3] === 0) continue;
			left = Math.min(left, x);
			top = Math.min(top, y);
			right = Math.max(right, x + 1);
			bottom = Math.max(bottom, y + 1);
		}
	}
	if (right <= left || bottom <= top) return null;
	const glyphWidth = right - left;
	const glyphHeight = bottom - top;
	const size = Math.max(glyphWidth, glyphHeight);
	const target = Buffer.alloc(size * size * 4);
	const offsetX = Math.floor((size - glyphWidth) / 2);
	const offsetY = Math.floor((size - glyphHeight) / 2);
	for (let y = 0; y < glyphHeight; y++) {
		const start = ((top + y) * width + left) * 4;
		buffer.copy(target, ((offsetY + y) * size + offsetX) * 4, start, start + glyphWidth * 4);
	}
	return {buffer: target, width: size, height: size};
}

function downscaleTrayBitmap(bitmap: TrayBitmap, size: number): TrayBitmap {
	if (bitmap.width <= size) return bitmap;
	const scale = bitmap.width / size;
	const buffer = Buffer.alloc(size * size * 4);
	const sums = [0, 0, 0, 0];
	for (let y = 0; y < size; y++) {
		const top = y * scale;
		const bottom = top + scale;
		for (let x = 0; x < size; x++) {
			const left = x * scale;
			const right = left + scale;
			sums.fill(0);
			for (let sy = Math.floor(top); sy < Math.min(bitmap.height, Math.ceil(bottom)); sy++) {
				const weightY = Math.min(bottom, sy + 1) - Math.max(top, sy);
				for (let sx = Math.floor(left); sx < Math.min(bitmap.width, Math.ceil(right)); sx++) {
					const weight = weightY * (Math.min(right, sx + 1) - Math.max(left, sx));
					const offset = (sy * bitmap.width + sx) * 4;
					for (let channel = 0; channel < 4; channel++) {
						sums[channel] += bitmap.buffer[offset + channel] * weight;
					}
				}
			}
			for (let channel = 0; channel < 4; channel++) {
				buffer[(y * size + x) * 4 + channel] = Math.round(sums[channel] / (scale * scale));
			}
		}
	}
	return {buffer, width: size, height: size};
}

function addMonochromeTrayMargins(bitmap: TrayBitmap, size: number): TrayBitmap {
	const inset = Math.floor((size - bitmap.width) / 2);
	const buffer = Buffer.alloc(size * size * 4);
	for (let y = 0; y < bitmap.height; y++) {
		bitmap.buffer.copy(buffer, ((inset + y) * size + inset) * 4, y * bitmap.width * 4, (y + 1) * bitmap.width * 4);
	}
	return {buffer, width: size, height: size};
}

export function renderTrayIcon(
	base: NativeImage,
	appearance: TrayIconAppearance,
	indicator: boolean,
	platform: NodeJS.Platform,
): NativeImage {
	const monochrome = appearance === 'white' || appearance === 'black';
	if (!monochrome && !indicator) {
		return base;
	}
	const result = nativeImage.createEmpty();
	const scaleFactors = base.getScaleFactors();
	const linuxMonochrome = platform === 'linux' && monochrome;
	const representations: Array<{scaleFactor: number; bitmap: TrayBitmap}> = [];
	for (const scaleFactor of scaleFactors.length > 0 ? scaleFactors : [1]) {
		if (!Number.isFinite(scaleFactor) || scaleFactor <= 0) continue;
		const size = base.getSize(scaleFactor);
		let width = Math.round(size.width * scaleFactor);
		let height = Math.round(size.height * scaleFactor);
		const buffer = Buffer.from(base.toBitmap({scaleFactor}));
		if (buffer.length === 0 || buffer.length % 4 !== 0) continue;
		if (buffer.length !== width * height * 4) {
			const png = base.toPNG({scaleFactor});
			if (png.length < 24 || !png.subarray(0, 8).equals(PNG_SIGNATURE) || png.toString('ascii', 12, 16) !== 'IHDR') {
				continue;
			}
			width = png.readUInt32BE(16);
			height = png.readUInt32BE(20);
		}
		if (width <= 0 || height <= 0 || buffer.length !== width * height * 4) continue;
		const bitmap = linuxMonochrome ? cropTrayGlyph(buffer, width, height) : {buffer, width, height};
		if (!bitmap) continue;
		representations.push({scaleFactor, bitmap});
	}
	if (representations.length === 0) return base;
	const logicalCanvasSize = Math.ceil(
		Math.max(...representations.map(({scaleFactor, bitmap}) => bitmap.width / scaleFactor)) *
			LINUX_MONOCHROME_CANVAS_RATIO,
	);
	for (const {scaleFactor, bitmap: source} of representations) {
		let bitmap = source;
		if (monochrome) {
			paintMonochromeGlyph(bitmap.buffer, GLYPH_BGR[appearance]);
		}
		if (indicator) {
			paintTrayIndicatorDot(
				bitmap.buffer,
				bitmap.width,
				bitmap.height,
				appearance === 'template' ? TEMPLATE_DOT_BGR : DOT_BGR,
			);
		}
		if (linuxMonochrome) {
			const canvasSize = Math.min(Math.ceil(logicalCanvasSize * scaleFactor), LINUX_MAX_PIXMAP_SIZE);
			bitmap = addMonochromeTrayMargins(
				downscaleTrayBitmap(bitmap, Math.floor(canvasSize / LINUX_MONOCHROME_CANVAS_RATIO)),
				canvasSize,
			);
		}
		result.addRepresentation({scaleFactor, ...bitmap});
	}
	if (result.isEmpty()) {
		return base;
	}
	result.setTemplateImage(appearance === 'template');
	return result;
}
