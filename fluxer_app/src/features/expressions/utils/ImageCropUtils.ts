// SPDX-License-Identifier: AGPL-3.0-or-later

import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import {readBlobAsBase64NoPrefix} from '@app/features/platform/utils/BlobDataUrl';
import {EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';

const EMOJI_MAX_SIZE_FALLBACK = EMOJI_MAX_SIZE;
const STICKER_MAX_SIZE_FALLBACK = STICKER_MAX_SIZE;
const SOURCE_MAX_PIXELS = 32_000_000;
const WEBP_QUALITY_STEPS = [0.92, 0.82, 0.72, 0.62, 0.52, 0.42];
const JPEG_QUALITY_STEPS = [0.9, 0.8, 0.7, 0.6, 0.5, 0.4];

export type ImageOptimizationSizeErrorReason = 'animated' | 'processed' | 'svg';
type StaticOutputMime = 'image/png' | 'image/webp' | 'image/jpeg';

export class ImageOptimizationSizeError extends Error {
	readonly actualSizeBytes: number;
	readonly maxSizeBytes: number;
	readonly reason: ImageOptimizationSizeErrorReason;

	constructor(reason: ImageOptimizationSizeErrorReason, actualSizeBytes: number, maxSizeBytes: number) {
		super(
			`Image size ${formatBytesForError(actualSizeBytes)} exceeds size limit of ${formatBytesForError(maxSizeBytes)}`,
		);
		this.name = 'ImageOptimizationSizeError';
		this.reason = reason;
		this.actualSizeBytes = actualSizeBytes;
		this.maxSizeBytes = maxSizeBytes;
	}
}

function formatBytesForError(bytes: number): string {
	if (bytes < 1024) return `${bytes} B`;
	return `${(bytes / 1024).toFixed(1)} KB`;
}

export async function optimizeEmojiImage(
	file: File,
	maxSizeBytes: number = EMOJI_MAX_SIZE_FALLBACK,
	targetSize = 128,
): Promise<string> {
	if (isSvgFile(file)) {
		if (file.size <= maxSizeBytes) {
			return readImageFileAsUploadDataUrl(file);
		}
		throw new ImageOptimizationSizeError('svg', file.size, maxSizeBytes);
	}
	if (await isAnimatedFile(file)) {
		if (file.size <= maxSizeBytes) {
			return readBlobAsBase64NoPrefix(file);
		}
		throw new ImageOptimizationSizeError('animated', file.size, maxSizeBytes);
	}
	return containToSquareBase64(file, targetSize, maxSizeBytes, 'image/png');
}

export async function optimizeStickerImage(
	file: File,
	maxSizeBytes: number = STICKER_MAX_SIZE_FALLBACK,
	targetSize = 320,
): Promise<string> {
	return optimizeEmojiImage(file, maxSizeBytes, targetSize);
}

function loadImageElement(src: string): Promise<HTMLImageElement> {
	return new Promise((resolve, reject) => {
		const image = new Image();
		image.onload = () => resolve(image);
		image.onerror = () => reject(new Error('Failed to load image'));
		image.src = src;
	});
}

async function containToSquareBase64(
	file: File,
	target: number,
	maxBytes: number,
	preferredMime: StaticOutputMime,
): Promise<string> {
	const canvas = document.createElement('canvas');
	canvas.width = target;
	canvas.height = target;
	const ctx = canvas.getContext('2d');
	if (!ctx) throw new Error('Could not create canvas context');
	const objectUrl = URL.createObjectURL(file);
	try {
		const img = await loadImageElement(objectUrl);
		const sourcePixels = img.naturalWidth * img.naturalHeight;
		if (sourcePixels === 0) throw new Error('Failed to load image');
		if (sourcePixels > SOURCE_MAX_PIXELS) {
			throw new Error(`Image is too large to process (${img.naturalWidth}x${img.naturalHeight})`);
		}
		ctx.clearRect(0, 0, target, target);
		ctx.imageSmoothingEnabled = true;
		ctx.imageSmoothingQuality = 'high';
		const s = Math.min(target / img.naturalWidth, target / img.naturalHeight);
		const dw = Math.max(1, Math.round(img.naturalWidth * s));
		const dh = Math.max(1, Math.round(img.naturalHeight * s));
		const dx = Math.floor((target - dw) / 2);
		const dy = Math.floor((target - dh) / 2);
		ctx.drawImage(img, 0, 0, img.naturalWidth, img.naturalHeight, dx, dy, dw, dh);
	} finally {
		URL.revokeObjectURL(objectUrl);
	}

	return readBlobAsBase64NoPrefix(await encodeCanvasWithinBytes(canvas, ctx, preferredMime, maxBytes));
}

export async function fitImageWithin(file: File, maxSide: number, maxBytes: number): Promise<Blob> {
	const objectUrl = URL.createObjectURL(file);
	let img: HTMLImageElement;
	try {
		img = await loadImageElement(objectUrl);
	} finally {
		URL.revokeObjectURL(objectUrl);
	}
	const {naturalWidth: width, naturalHeight: height} = img;
	if (width === 0 || height === 0) throw new Error('Failed to load image');
	if (Math.max(width, height) <= maxSide && file.size <= maxBytes) return file;
	if (width * height > SOURCE_MAX_PIXELS) {
		throw new Error(`Image is too large to process (${width}x${height})`);
	}
	const scale = Math.min(1, maxSide / Math.max(width, height));
	const canvas = document.createElement('canvas');
	canvas.width = Math.max(1, Math.round(width * scale));
	canvas.height = Math.max(1, Math.round(height * scale));
	const ctx = canvas.getContext('2d');
	if (!ctx) throw new Error('Could not create canvas context');
	ctx.imageSmoothingEnabled = true;
	ctx.imageSmoothingQuality = 'high';
	ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
	return encodeCanvasWithinBytes(canvas, ctx, 'image/webp', maxBytes);
}

async function encodeCanvasWithinBytes(
	canvas: HTMLCanvasElement,
	ctx: CanvasRenderingContext2D,
	preferredMime: StaticOutputMime,
	maxBytes: number,
): Promise<Blob> {
	const hasTransparency = canvasHasTransparentPixels(ctx, canvas.width, canvas.height);
	const attempts: Array<{mime: StaticOutputMime; quality?: number}> =
		preferredMime === 'image/webp' ? [] : [{mime: preferredMime}];
	for (const quality of WEBP_QUALITY_STEPS) attempts.push({mime: 'image/webp', quality});
	if (!hasTransparency) {
		for (const quality of JPEG_QUALITY_STEPS) attempts.push({mime: 'image/jpeg', quality});
	}

	let smallestBlob: Blob | null = null;
	for (const attempt of attempts) {
		const blob = await encodeCanvas(canvas, attempt.mime, attempt.quality);
		if (!blob) continue;
		if (blob.type && blob.type !== attempt.mime) continue;
		if (!smallestBlob || blob.size < smallestBlob.size) {
			smallestBlob = blob;
		}
		if (blob.size <= maxBytes) {
			return blob;
		}
	}
	if (smallestBlob) {
		throw new ImageOptimizationSizeError('processed', smallestBlob.size, maxBytes);
	}
	throw new Error('Canvas toBlob failed');
}

function encodeCanvas(canvas: HTMLCanvasElement, mime: StaticOutputMime, quality?: number): Promise<Blob | null> {
	return new Promise((resolve) => {
		canvas.toBlob((blob) => resolve(blob), mime, quality);
	});
}

function canvasHasTransparentPixels(ctx: CanvasRenderingContext2D, width: number, height: number): boolean {
	try {
		const {data} = ctx.getImageData(0, 0, width, height);
		for (let i = 3; i < data.length; i += 4) {
			if ((data[i] ?? 255) < 255) return true;
		}
	} catch {
		return true;
	}
	return false;
}
