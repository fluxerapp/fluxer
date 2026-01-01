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

export const MAX_IMAGE_BYTES = 10 * 1024 * 1024;

export function isGif(file: File): boolean {
	const type = (file.type || '').toLowerCase();
	if (type === 'image/gif') return true;

	const name = (file.name || '').toLowerCase();
	return name.endsWith('.gif');
}

export function revokeObjectUrl(url: string | null | undefined): void {
	if (!url) return;
	if (!url.startsWith('blob:')) return;
	try {
		URL.revokeObjectURL(url);
	} catch {}
}

export function blobToDataUrl(blob: Blob): Promise<string> {
	return new Promise((resolve, reject) => {
		const reader = new FileReader();
		reader.onload = () => resolve(reader.result as string);
		reader.onerror = () => reject(new Error('Failed to read blob as data URL'));
		reader.readAsDataURL(blob);
	});
}

export function getSafeImageMimeType(file: File): string {
	const type = (file.type || '').toLowerCase();
	if (type.startsWith('image/')) return type;
	return 'image/png';
}
