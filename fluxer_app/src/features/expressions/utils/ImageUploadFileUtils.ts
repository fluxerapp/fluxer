// SPDX-License-Identifier: AGPL-3.0-or-later

import {readBlobAsDataUrl} from '@app/features/platform/utils/BlobDataUrl';
import {type AssetKind, getPolicy, isExtensionAllowed} from '@fluxer/constants/src/AssetFormatPolicy';

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

export function isAssetUploadFileAllowed(kind: AssetKind, file: ImageUploadFileLike): boolean {
	const name = (file.name ?? '').toLowerCase();
	const dot = name.lastIndexOf('.');
	if (dot !== -1 && isExtensionAllowed(kind, name.slice(dot + 1))) return true;
	const mime = normalizeImageMimeType(file.type);
	return mime !== '' && Object.values(getPolicy(kind).mimes).includes(mime);
}

export async function readImageFileAsUploadDataUrl(file: File): Promise<string> {
	const dataUrl = await readBlobAsDataUrl(file);
	if (!isSvgFile(file)) return dataUrl;
	const base64Data = dataUrl.split(',')[1] ?? '';
	return `data:image/svg+xml;base64,${base64Data}`;
}
