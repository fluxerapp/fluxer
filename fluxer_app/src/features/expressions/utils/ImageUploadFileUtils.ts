// SPDX-License-Identifier: AGPL-3.0-or-later

import {readBlobAsDataUrl} from '@app/features/platform/utils/BlobDataUrl';

export interface ImageUploadFileLike {
	readonly type?: string | null;
	readonly name?: string | null;
}

export function normalizeImageMimeType(type: string | null | undefined): string {
	return (type ?? '').toLowerCase().split(';', 1)[0]?.trim() ?? '';
}

export function isSvgMimeType(type: string | null | undefined): boolean {
	const mime = normalizeImageMimeType(type);
	return mime === 'image/svg+xml' || mime === 'image/svg';
}

export function isSvgFile(file: ImageUploadFileLike): boolean {
	if (isSvgMimeType(file.type)) return true;
	return (file.name ?? '').toLowerCase().endsWith('.svg');
}

export async function readImageFileAsUploadDataUrl(file: File): Promise<string> {
	const dataUrl = await readBlobAsDataUrl(file);
	if (!isSvgFile(file)) return dataUrl;
	const base64Data = dataUrl.split(',')[1] ?? '';
	return `data:image/svg+xml;base64,${base64Data}`;
}
