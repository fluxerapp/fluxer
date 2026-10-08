// SPDX-License-Identifier: AGPL-3.0-or-later

export const MAX_IMAGE_BYTES = 10 * 1024 * 1024;

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
