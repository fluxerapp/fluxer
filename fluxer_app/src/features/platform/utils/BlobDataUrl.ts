// SPDX-License-Identifier: AGPL-3.0-or-later

export function readBlobAsDataUrl(blob: Blob): Promise<string> {
	return new Promise((resolve, reject) => {
		const reader = new FileReader();
		reader.onload = () => resolve(typeof reader.result === 'string' ? reader.result : '');
		reader.onerror = () => reject(reader.error ?? new Error('Failed to read blob as data URL'));
		reader.readAsDataURL(blob);
	});
}
export async function readBlobAsBase64NoPrefix(blob: Blob): Promise<string> {
	const dataUrl = await readBlobAsDataUrl(blob);
	return dataUrl.split(',')[1] ?? '';
}
