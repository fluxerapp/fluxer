// SPDX-License-Identifier: AGPL-3.0-or-later

function sanitizeOptionalAbsoluteUrl(url: string | null | undefined): string | undefined {
	if (typeof url !== 'string') {
		return;
	}
	const trimmedUrl = url.trim();
	if (!trimmedUrl) {
		return;
	}
	try {
		const parsedUrl = new URL(trimmedUrl);
		if (parsedUrl.protocol !== 'http:' && parsedUrl.protocol !== 'https:') {
			return;
		}
		return parsedUrl.toString();
	} catch {
		return;
	}
}

export function sanitizeOptionalAbsoluteUrlOrNull(url: string | null | undefined): string | null {
	return sanitizeOptionalAbsoluteUrl(url) ?? null;
}
