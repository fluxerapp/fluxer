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

import {EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '~/Constants';

export async function optimizeEmojiImage(file: File, maxSizeBytes = EMOJI_MAX_SIZE, targetSize = 128): Promise<string> {
	const isGif = file.type === 'image/gif';

	if (isGif) {
		if (file.size <= maxSizeBytes) {
			return fileToBase64NoPrefix(file);
		}
		throw new Error('Animated GIF exceeds size limit and cannot be compressed further');
	}

	return containToSquareBase64(file, targetSize, maxSizeBytes, 'image/png');
}

export async function optimizeStickerImage(
	file: File,
	maxSizeBytes = STICKER_MAX_SIZE,
	targetSize = 320,
): Promise<string> {
	return optimizeEmojiImage(file, maxSizeBytes, targetSize);
}

async function fileToBase64NoPrefix(file: File): Promise<string> {
	const dataUrl = await new Promise<string>((res, rej) => {
		const r = new FileReader();
		r.onload = () => res(String(r.result));
		r.onerror = () => rej(new Error('Failed to read file'));
		r.readAsDataURL(file);
	});
	return dataUrl.split(',')[1] ?? '';
}

async function containToSquareBase64(
	file: File,
	target: number,
	maxBytes: number,
	mime: 'image/png' | 'image/webp' | 'image/jpeg',
): Promise<string> {
	const dataUrl = await new Promise<string>((res, rej) => {
		const r = new FileReader();
		r.onload = () => res(String(r.result));
		r.onerror = () => rej(new Error('Failed to read file'));
		r.readAsDataURL(file);
	});

	const img = await new Promise<HTMLImageElement>((resolve, reject) => {
		const im = new Image();
		im.crossOrigin = 'anonymous';
		im.onload = () => resolve(im);
		im.onerror = () => reject(new Error('Failed to load image'));
		im.src = dataUrl;
	});

	const canvas = document.createElement('canvas');
	canvas.width = target;
	canvas.height = target;
	const ctx = canvas.getContext('2d');
	if (!ctx) throw new Error('Could not create canvas context');

	ctx.clearRect(0, 0, target, target);
	ctx.imageSmoothingEnabled = true;
	ctx.imageSmoothingQuality = 'high';

	const s = Math.min(target / img.width, target / img.height);
	const dw = Math.max(1, Math.round(img.width * s));
	const dh = Math.max(1, Math.round(img.height * s));
	const dx = Math.floor((target - dw) / 2);
	const dy = Math.floor((target - dh) / 2);

	ctx.drawImage(img, 0, 0, img.width, img.height, dx, dy, dw, dh);

	const blob: Blob = await new Promise((res, rej) =>
		canvas.toBlob((b) => (b ? res(b) : rej(new Error('Canvas toBlob failed'))), mime, 0.95),
	);
	if (blob.size > maxBytes) {
		throw new Error(`Image size ${(blob.size / 1024).toFixed(1)} KB exceeds max ${(maxBytes / 1024).toFixed(0)} KB`);
	}

	const arr = new Uint8Array(await blob.arrayBuffer());
	let bin = '';
	for (let i = 0; i < arr.length; i++) bin += String.fromCharCode(arr[i]);
	return btoa(bin);
}
