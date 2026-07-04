// SPDX-License-Identifier: AGPL-3.0-or-later

export function resolveActivityImageUrl(image: string | undefined, _applicationId?: string): string | null {
	if (!image) return null;
	if (image.startsWith('http://') || image.startsWith('https://')) return image;
	return null;
}
